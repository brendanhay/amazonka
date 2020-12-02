{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ProxyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ProxyConfiguration where

import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ProxyConfigurationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration details for the App Mesh proxy.
--
--
-- For tasks using the EC2 launch type, the container instances require at least version 1.26.0 of the container agent and at least version 1.26.0-1 of the @ecs-init@ package to enable a proxy configuration. If your container instances are launched from the Amazon ECS-optimized AMI version @20190301@ or later, then they contain the required versions of the container agent and @ecs-init@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
--
--
-- /See:/ 'proxyConfiguration' smart constructor.
data ProxyConfiguration = ProxyConfiguration'
  { _pType ::
      !(Maybe ProxyConfigurationType),
    _pProperties :: !(Maybe [KeyValuePair]),
    _pContainerName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProxyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pType' - The proxy type. The only supported value is @APPMESH@ .
--
-- * 'pProperties' - The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredUID@ is specified, this field can be empty.     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports is forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.     * @EgressIgnoredPorts@ - (Required) The egress traffic going to the specified ports is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.     * @EgressIgnoredIPs@ - (Required) The egress traffic going to the specified IP addresses is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
--
-- * 'pContainerName' - The name of the container that will serve as the App Mesh proxy.
proxyConfiguration ::
  -- | 'pContainerName'
  Text ->
  ProxyConfiguration
proxyConfiguration pContainerName_ =
  ProxyConfiguration'
    { _pType = Nothing,
      _pProperties = Nothing,
      _pContainerName = pContainerName_
    }

-- | The proxy type. The only supported value is @APPMESH@ .
pType :: Lens' ProxyConfiguration (Maybe ProxyConfigurationType)
pType = lens _pType (\s a -> s {_pType = a})

-- | The set of network configuration parameters to provide the Container Network Interface (CNI) plugin, specified as key-value pairs.     * @IgnoredUID@ - (Required) The user ID (UID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredGID@ is specified, this field can be empty.     * @IgnoredGID@ - (Required) The group ID (GID) of the proxy container as defined by the @user@ parameter in a container definition. This is used to ensure the proxy ignores its own traffic. If @IgnoredUID@ is specified, this field can be empty.     * @AppPorts@ - (Required) The list of ports that the application uses. Network traffic to these ports is forwarded to the @ProxyIngressPort@ and @ProxyEgressPort@ .     * @ProxyIngressPort@ - (Required) Specifies the port that incoming traffic to the @AppPorts@ is directed to.     * @ProxyEgressPort@ - (Required) Specifies the port that outgoing traffic from the @AppPorts@ is directed to.     * @EgressIgnoredPorts@ - (Required) The egress traffic going to the specified ports is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.     * @EgressIgnoredIPs@ - (Required) The egress traffic going to the specified IP addresses is ignored and not redirected to the @ProxyEgressPort@ . It can be an empty list.
pProperties :: Lens' ProxyConfiguration [KeyValuePair]
pProperties = lens _pProperties (\s a -> s {_pProperties = a}) . _Default . _Coerce

-- | The name of the container that will serve as the App Mesh proxy.
pContainerName :: Lens' ProxyConfiguration Text
pContainerName = lens _pContainerName (\s a -> s {_pContainerName = a})

instance FromJSON ProxyConfiguration where
  parseJSON =
    withObject
      "ProxyConfiguration"
      ( \x ->
          ProxyConfiguration'
            <$> (x .:? "type")
            <*> (x .:? "properties" .!= mempty)
            <*> (x .: "containerName")
      )

instance Hashable ProxyConfiguration

instance NFData ProxyConfiguration

instance ToJSON ProxyConfiguration where
  toJSON ProxyConfiguration' {..} =
    object
      ( catMaybes
          [ ("type" .=) <$> _pType,
            ("properties" .=) <$> _pProperties,
            Just ("containerName" .= _pContainerName)
          ]
      )
