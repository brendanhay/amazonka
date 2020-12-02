{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig

-- | A complex type that contains information about the specified service.
--
--
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _siInstanceCount :: !(Maybe Int),
    _siARN :: !(Maybe Text),
    _siHealthCheckConfig :: !(Maybe HealthCheckConfig),
    _siCreatorRequestId :: !(Maybe Text),
    _siCreateDate :: !(Maybe POSIX),
    _siHealthCheckCustomConfig :: !(Maybe HealthCheckCustomConfig),
    _siNamespaceId :: !(Maybe Text),
    _siName :: !(Maybe Text),
    _siId :: !(Maybe Text),
    _siDNSConfig :: !(Maybe DNSConfig),
    _siDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siInstanceCount' - The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
--
-- * 'siARN' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
--
-- * 'siHealthCheckConfig' - /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
--
-- * 'siCreatorRequestId' - A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'siCreateDate' - The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'siHealthCheckCustomConfig' - A complex type that contains information about an optional custom health check. /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- * 'siNamespaceId' - The ID of the namespace that was used to create the service.
--
-- * 'siName' - The name of the service.
--
-- * 'siId' - The ID that AWS Cloud Map assigned to the service when you created it.
--
-- * 'siDNSConfig' - A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- * 'siDescription' - The description of the service.
serviceInfo ::
  ServiceInfo
serviceInfo =
  ServiceInfo'
    { _siInstanceCount = Nothing,
      _siARN = Nothing,
      _siHealthCheckConfig = Nothing,
      _siCreatorRequestId = Nothing,
      _siCreateDate = Nothing,
      _siHealthCheckCustomConfig = Nothing,
      _siNamespaceId = Nothing,
      _siName = Nothing,
      _siId = Nothing,
      _siDNSConfig = Nothing,
      _siDescription = Nothing
    }

-- | The number of instances that are currently associated with the service. Instances that were previously associated with the service but that have been deleted are not included in the count. The count might not reflect pending registrations and deregistrations.
siInstanceCount :: Lens' ServiceInfo (Maybe Int)
siInstanceCount = lens _siInstanceCount (\s a -> s {_siInstanceCount = a})

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service when you create it.
siARN :: Lens' ServiceInfo (Maybe Text)
siARN = lens _siARN (\s a -> s {_siARN = a})

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional health check. If you specify settings for a health check, AWS Cloud Map associates the health check with the records that you specify in @DnsConfig@ . For information about the charges for health checks, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
siHealthCheckConfig :: Lens' ServiceInfo (Maybe HealthCheckConfig)
siHealthCheckConfig = lens _siHealthCheckConfig (\s a -> s {_siHealthCheckConfig = a})

-- | A unique string that identifies the request and that allows failed requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
siCreatorRequestId :: Lens' ServiceInfo (Maybe Text)
siCreatorRequestId = lens _siCreatorRequestId (\s a -> s {_siCreatorRequestId = a})

-- | The date and time that the service was created, in Unix format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
siCreateDate :: Lens' ServiceInfo (Maybe UTCTime)
siCreateDate = lens _siCreateDate (\s a -> s {_siCreateDate = a}) . mapping _Time

-- | A complex type that contains information about an optional custom health check. /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
siHealthCheckCustomConfig :: Lens' ServiceInfo (Maybe HealthCheckCustomConfig)
siHealthCheckCustomConfig = lens _siHealthCheckCustomConfig (\s a -> s {_siHealthCheckCustomConfig = a})

-- | The ID of the namespace that was used to create the service.
siNamespaceId :: Lens' ServiceInfo (Maybe Text)
siNamespaceId = lens _siNamespaceId (\s a -> s {_siNamespaceId = a})

-- | The name of the service.
siName :: Lens' ServiceInfo (Maybe Text)
siName = lens _siName (\s a -> s {_siName = a})

-- | The ID that AWS Cloud Map assigned to the service when you created it.
siId :: Lens' ServiceInfo (Maybe Text)
siId = lens _siId (\s a -> s {_siId = a})

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
siDNSConfig :: Lens' ServiceInfo (Maybe DNSConfig)
siDNSConfig = lens _siDNSConfig (\s a -> s {_siDNSConfig = a})

-- | The description of the service.
siDescription :: Lens' ServiceInfo (Maybe Text)
siDescription = lens _siDescription (\s a -> s {_siDescription = a})

instance FromJSON ServiceInfo where
  parseJSON =
    withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            <$> (x .:? "InstanceCount")
            <*> (x .:? "Arn")
            <*> (x .:? "HealthCheckConfig")
            <*> (x .:? "CreatorRequestId")
            <*> (x .:? "CreateDate")
            <*> (x .:? "HealthCheckCustomConfig")
            <*> (x .:? "NamespaceId")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "DnsConfig")
            <*> (x .:? "Description")
      )

instance Hashable ServiceInfo

instance NFData ServiceInfo
