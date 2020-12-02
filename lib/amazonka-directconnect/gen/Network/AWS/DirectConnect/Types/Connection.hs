{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connection where

import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an AWS Direct Connect connection.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _cLagId :: !(Maybe Text),
    _cVlan :: !(Maybe Int),
    _cLocation :: !(Maybe Text),
    _cAwsDevice :: !(Maybe Text),
    _cHasLogicalRedundancy :: !(Maybe HasLogicalRedundancy),
    _cConnectionId :: !(Maybe Text),
    _cLoaIssueTime :: !(Maybe POSIX),
    _cPartnerName :: !(Maybe Text),
    _cConnectionName :: !(Maybe Text),
    _cBandwidth :: !(Maybe Text),
    _cJumboFrameCapable :: !(Maybe Bool),
    _cOwnerAccount :: !(Maybe Text),
    _cRegion :: !(Maybe Text),
    _cProviderName :: !(Maybe Text),
    _cAwsDeviceV2 :: !(Maybe Text),
    _cConnectionState :: !(Maybe ConnectionState),
    _cTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLagId' - The ID of the LAG.
--
-- * 'cVlan' - The ID of the VLAN.
--
-- * 'cLocation' - The location of the connection.
--
-- * 'cAwsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'cHasLogicalRedundancy' - Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- * 'cConnectionId' - The ID of the connection.
--
-- * 'cLoaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
--
-- * 'cPartnerName' - The name of the AWS Direct Connect service provider associated with the connection.
--
-- * 'cConnectionName' - The name of the connection.
--
-- * 'cBandwidth' - The bandwidth of the connection.
--
-- * 'cJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'cOwnerAccount' - The ID of the AWS account that owns the connection.
--
-- * 'cRegion' - The AWS Region where the connection is located.
--
-- * 'cProviderName' - The name of the service provider associated with the connection.
--
-- * 'cAwsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'cConnectionState' - The state of the connection. The following are the possible values:     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The connection has been approved and is being initialized.     * @available@ : The network link is up and the connection is ready for use.     * @down@ : The network link is down.     * @deleting@ : The connection is being deleted.     * @deleted@ : The connection has been deleted.     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.     * @unknown@ : The state of the connection is not available.
--
-- * 'cTags' - The tags associated with the connection.
connection ::
  Connection
connection =
  Connection'
    { _cLagId = Nothing,
      _cVlan = Nothing,
      _cLocation = Nothing,
      _cAwsDevice = Nothing,
      _cHasLogicalRedundancy = Nothing,
      _cConnectionId = Nothing,
      _cLoaIssueTime = Nothing,
      _cPartnerName = Nothing,
      _cConnectionName = Nothing,
      _cBandwidth = Nothing,
      _cJumboFrameCapable = Nothing,
      _cOwnerAccount = Nothing,
      _cRegion = Nothing,
      _cProviderName = Nothing,
      _cAwsDeviceV2 = Nothing,
      _cConnectionState = Nothing,
      _cTags = Nothing
    }

-- | The ID of the LAG.
cLagId :: Lens' Connection (Maybe Text)
cLagId = lens _cLagId (\s a -> s {_cLagId = a})

-- | The ID of the VLAN.
cVlan :: Lens' Connection (Maybe Int)
cVlan = lens _cVlan (\s a -> s {_cVlan = a})

-- | The location of the connection.
cLocation :: Lens' Connection (Maybe Text)
cLocation = lens _cLocation (\s a -> s {_cLocation = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
cAwsDevice :: Lens' Connection (Maybe Text)
cAwsDevice = lens _cAwsDevice (\s a -> s {_cAwsDevice = a})

-- | Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
cHasLogicalRedundancy :: Lens' Connection (Maybe HasLogicalRedundancy)
cHasLogicalRedundancy = lens _cHasLogicalRedundancy (\s a -> s {_cHasLogicalRedundancy = a})

-- | The ID of the connection.
cConnectionId :: Lens' Connection (Maybe Text)
cConnectionId = lens _cConnectionId (\s a -> s {_cConnectionId = a})

-- | The time of the most recent call to 'DescribeLoa' for this connection.
cLoaIssueTime :: Lens' Connection (Maybe UTCTime)
cLoaIssueTime = lens _cLoaIssueTime (\s a -> s {_cLoaIssueTime = a}) . mapping _Time

-- | The name of the AWS Direct Connect service provider associated with the connection.
cPartnerName :: Lens' Connection (Maybe Text)
cPartnerName = lens _cPartnerName (\s a -> s {_cPartnerName = a})

-- | The name of the connection.
cConnectionName :: Lens' Connection (Maybe Text)
cConnectionName = lens _cConnectionName (\s a -> s {_cConnectionName = a})

-- | The bandwidth of the connection.
cBandwidth :: Lens' Connection (Maybe Text)
cBandwidth = lens _cBandwidth (\s a -> s {_cBandwidth = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
cJumboFrameCapable :: Lens' Connection (Maybe Bool)
cJumboFrameCapable = lens _cJumboFrameCapable (\s a -> s {_cJumboFrameCapable = a})

-- | The ID of the AWS account that owns the connection.
cOwnerAccount :: Lens' Connection (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\s a -> s {_cOwnerAccount = a})

-- | The AWS Region where the connection is located.
cRegion :: Lens' Connection (Maybe Text)
cRegion = lens _cRegion (\s a -> s {_cRegion = a})

-- | The name of the service provider associated with the connection.
cProviderName :: Lens' Connection (Maybe Text)
cProviderName = lens _cProviderName (\s a -> s {_cProviderName = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
cAwsDeviceV2 :: Lens' Connection (Maybe Text)
cAwsDeviceV2 = lens _cAwsDeviceV2 (\s a -> s {_cAwsDeviceV2 = a})

-- | The state of the connection. The following are the possible values:     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The connection has been approved and is being initialized.     * @available@ : The network link is up and the connection is ready for use.     * @down@ : The network link is down.     * @deleting@ : The connection is being deleted.     * @deleted@ : The connection has been deleted.     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.     * @unknown@ : The state of the connection is not available.
cConnectionState :: Lens' Connection (Maybe ConnectionState)
cConnectionState = lens _cConnectionState (\s a -> s {_cConnectionState = a})

-- | The tags associated with the connection.
cTags :: Lens' Connection (Maybe (NonEmpty Tag))
cTags = lens _cTags (\s a -> s {_cTags = a}) . mapping _List1

instance FromJSON Connection where
  parseJSON =
    withObject
      "Connection"
      ( \x ->
          Connection'
            <$> (x .:? "lagId")
            <*> (x .:? "vlan")
            <*> (x .:? "location")
            <*> (x .:? "awsDevice")
            <*> (x .:? "hasLogicalRedundancy")
            <*> (x .:? "connectionId")
            <*> (x .:? "loaIssueTime")
            <*> (x .:? "partnerName")
            <*> (x .:? "connectionName")
            <*> (x .:? "bandwidth")
            <*> (x .:? "jumboFrameCapable")
            <*> (x .:? "ownerAccount")
            <*> (x .:? "region")
            <*> (x .:? "providerName")
            <*> (x .:? "awsDeviceV2")
            <*> (x .:? "connectionState")
            <*> (x .:? "tags")
      )

instance Hashable Connection

instance NFData Connection
