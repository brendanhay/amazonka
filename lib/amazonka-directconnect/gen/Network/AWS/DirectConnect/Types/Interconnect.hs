{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Interconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Interconnect where

import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.InterconnectState
import Network.AWS.DirectConnect.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an interconnect.
--
--
--
-- /See:/ 'interconnect' smart constructor.
data Interconnect = Interconnect'
  { _iLagId :: !(Maybe Text),
    _iInterconnectId :: !(Maybe Text),
    _iLocation :: !(Maybe Text),
    _iInterconnectName :: !(Maybe Text),
    _iAwsDevice :: !(Maybe Text),
    _iHasLogicalRedundancy :: !(Maybe HasLogicalRedundancy),
    _iLoaIssueTime :: !(Maybe POSIX),
    _iBandwidth :: !(Maybe Text),
    _iJumboFrameCapable :: !(Maybe Bool),
    _iInterconnectState :: !(Maybe InterconnectState),
    _iRegion :: !(Maybe Text),
    _iProviderName :: !(Maybe Text),
    _iAwsDeviceV2 :: !(Maybe Text),
    _iTags :: !(Maybe (List1 Tag))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Interconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iLagId' - The ID of the LAG.
--
-- * 'iInterconnectId' - The ID of the interconnect.
--
-- * 'iLocation' - The location of the connection.
--
-- * 'iInterconnectName' - The name of the interconnect.
--
-- * 'iAwsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'iHasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
--
-- * 'iLoaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
--
-- * 'iBandwidth' - The bandwidth of the connection.
--
-- * 'iJumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- * 'iInterconnectState' - The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
--
-- * 'iRegion' - The AWS Region where the connection is located.
--
-- * 'iProviderName' - The name of the service provider associated with the interconnect.
--
-- * 'iAwsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
--
-- * 'iTags' - The tags associated with the interconnect.
interconnect ::
  Interconnect
interconnect =
  Interconnect'
    { _iLagId = Nothing,
      _iInterconnectId = Nothing,
      _iLocation = Nothing,
      _iInterconnectName = Nothing,
      _iAwsDevice = Nothing,
      _iHasLogicalRedundancy = Nothing,
      _iLoaIssueTime = Nothing,
      _iBandwidth = Nothing,
      _iJumboFrameCapable = Nothing,
      _iInterconnectState = Nothing,
      _iRegion = Nothing,
      _iProviderName = Nothing,
      _iAwsDeviceV2 = Nothing,
      _iTags = Nothing
    }

-- | The ID of the LAG.
iLagId :: Lens' Interconnect (Maybe Text)
iLagId = lens _iLagId (\s a -> s {_iLagId = a})

-- | The ID of the interconnect.
iInterconnectId :: Lens' Interconnect (Maybe Text)
iInterconnectId = lens _iInterconnectId (\s a -> s {_iInterconnectId = a})

-- | The location of the connection.
iLocation :: Lens' Interconnect (Maybe Text)
iLocation = lens _iLocation (\s a -> s {_iLocation = a})

-- | The name of the interconnect.
iInterconnectName :: Lens' Interconnect (Maybe Text)
iInterconnectName = lens _iInterconnectName (\s a -> s {_iInterconnectName = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
iAwsDevice :: Lens' Interconnect (Maybe Text)
iAwsDevice = lens _iAwsDevice (\s a -> s {_iAwsDevice = a})

-- | Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
iHasLogicalRedundancy :: Lens' Interconnect (Maybe HasLogicalRedundancy)
iHasLogicalRedundancy = lens _iHasLogicalRedundancy (\s a -> s {_iHasLogicalRedundancy = a})

-- | The time of the most recent call to 'DescribeLoa' for this connection.
iLoaIssueTime :: Lens' Interconnect (Maybe UTCTime)
iLoaIssueTime = lens _iLoaIssueTime (\s a -> s {_iLoaIssueTime = a}) . mapping _Time

-- | The bandwidth of the connection.
iBandwidth :: Lens' Interconnect (Maybe Text)
iBandwidth = lens _iBandwidth (\s a -> s {_iBandwidth = a})

-- | Indicates whether jumbo frames (9001 MTU) are supported.
iJumboFrameCapable :: Lens' Interconnect (Maybe Bool)
iJumboFrameCapable = lens _iJumboFrameCapable (\s a -> s {_iJumboFrameCapable = a})

-- | The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
iInterconnectState :: Lens' Interconnect (Maybe InterconnectState)
iInterconnectState = lens _iInterconnectState (\s a -> s {_iInterconnectState = a})

-- | The AWS Region where the connection is located.
iRegion :: Lens' Interconnect (Maybe Text)
iRegion = lens _iRegion (\s a -> s {_iRegion = a})

-- | The name of the service provider associated with the interconnect.
iProviderName :: Lens' Interconnect (Maybe Text)
iProviderName = lens _iProviderName (\s a -> s {_iProviderName = a})

-- | The Direct Connect endpoint on which the physical connection terminates.
iAwsDeviceV2 :: Lens' Interconnect (Maybe Text)
iAwsDeviceV2 = lens _iAwsDeviceV2 (\s a -> s {_iAwsDeviceV2 = a})

-- | The tags associated with the interconnect.
iTags :: Lens' Interconnect (Maybe (NonEmpty Tag))
iTags = lens _iTags (\s a -> s {_iTags = a}) . mapping _List1

instance FromJSON Interconnect where
  parseJSON =
    withObject
      "Interconnect"
      ( \x ->
          Interconnect'
            <$> (x .:? "lagId")
            <*> (x .:? "interconnectId")
            <*> (x .:? "location")
            <*> (x .:? "interconnectName")
            <*> (x .:? "awsDevice")
            <*> (x .:? "hasLogicalRedundancy")
            <*> (x .:? "loaIssueTime")
            <*> (x .:? "bandwidth")
            <*> (x .:? "jumboFrameCapable")
            <*> (x .:? "interconnectState")
            <*> (x .:? "region")
            <*> (x .:? "providerName")
            <*> (x .:? "awsDeviceV2")
            <*> (x .:? "tags")
      )

instance Hashable Interconnect

instance NFData Interconnect
