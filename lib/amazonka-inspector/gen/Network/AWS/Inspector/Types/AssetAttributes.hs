{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssetAttributes where

import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of attributes of the host from which the finding is generated.
--
--
--
-- /See:/ 'assetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { _aaHostname ::
      !(Maybe Text),
    _aaAutoScalingGroup :: !(Maybe Text),
    _aaNetworkInterfaces :: !(Maybe [NetworkInterface]),
    _aaIpv4Addresses :: !(Maybe [Text]),
    _aaAgentId :: !(Maybe Text),
    _aaAmiId :: !(Maybe Text),
    _aaTags :: !(Maybe [Tag]),
    _aaSchemaVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaHostname' - The hostname of the EC2 instance where the finding is generated.
--
-- * 'aaAutoScalingGroup' - The Auto Scaling group of the EC2 instance where the finding is generated.
--
-- * 'aaNetworkInterfaces' - An array of the network interfaces interacting with the EC2 instance where the finding is generated.
--
-- * 'aaIpv4Addresses' - The list of IP v4 addresses of the EC2 instance where the finding is generated.
--
-- * 'aaAgentId' - The ID of the agent that is installed on the EC2 instance where the finding is generated.
--
-- * 'aaAmiId' - The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
--
-- * 'aaTags' - The tags related to the EC2 instance where the finding is generated.
--
-- * 'aaSchemaVersion' - The schema version of this data type.
assetAttributes ::
  -- | 'aaSchemaVersion'
  Natural ->
  AssetAttributes
assetAttributes pSchemaVersion_ =
  AssetAttributes'
    { _aaHostname = Nothing,
      _aaAutoScalingGroup = Nothing,
      _aaNetworkInterfaces = Nothing,
      _aaIpv4Addresses = Nothing,
      _aaAgentId = Nothing,
      _aaAmiId = Nothing,
      _aaTags = Nothing,
      _aaSchemaVersion = _Nat # pSchemaVersion_
    }

-- | The hostname of the EC2 instance where the finding is generated.
aaHostname :: Lens' AssetAttributes (Maybe Text)
aaHostname = lens _aaHostname (\s a -> s {_aaHostname = a})

-- | The Auto Scaling group of the EC2 instance where the finding is generated.
aaAutoScalingGroup :: Lens' AssetAttributes (Maybe Text)
aaAutoScalingGroup = lens _aaAutoScalingGroup (\s a -> s {_aaAutoScalingGroup = a})

-- | An array of the network interfaces interacting with the EC2 instance where the finding is generated.
aaNetworkInterfaces :: Lens' AssetAttributes [NetworkInterface]
aaNetworkInterfaces = lens _aaNetworkInterfaces (\s a -> s {_aaNetworkInterfaces = a}) . _Default . _Coerce

-- | The list of IP v4 addresses of the EC2 instance where the finding is generated.
aaIpv4Addresses :: Lens' AssetAttributes [Text]
aaIpv4Addresses = lens _aaIpv4Addresses (\s a -> s {_aaIpv4Addresses = a}) . _Default . _Coerce

-- | The ID of the agent that is installed on the EC2 instance where the finding is generated.
aaAgentId :: Lens' AssetAttributes (Maybe Text)
aaAgentId = lens _aaAgentId (\s a -> s {_aaAgentId = a})

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
aaAmiId :: Lens' AssetAttributes (Maybe Text)
aaAmiId = lens _aaAmiId (\s a -> s {_aaAmiId = a})

-- | The tags related to the EC2 instance where the finding is generated.
aaTags :: Lens' AssetAttributes [Tag]
aaTags = lens _aaTags (\s a -> s {_aaTags = a}) . _Default . _Coerce

-- | The schema version of this data type.
aaSchemaVersion :: Lens' AssetAttributes Natural
aaSchemaVersion = lens _aaSchemaVersion (\s a -> s {_aaSchemaVersion = a}) . _Nat

instance FromJSON AssetAttributes where
  parseJSON =
    withObject
      "AssetAttributes"
      ( \x ->
          AssetAttributes'
            <$> (x .:? "hostname")
            <*> (x .:? "autoScalingGroup")
            <*> (x .:? "networkInterfaces" .!= mempty)
            <*> (x .:? "ipv4Addresses" .!= mempty)
            <*> (x .:? "agentId")
            <*> (x .:? "amiId")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "schemaVersion")
      )

instance Hashable AssetAttributes

instance NFData AssetAttributes
