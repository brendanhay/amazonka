{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the virtual interface failover test.
--
--
--
-- /See:/ 'virtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { _vithBgpPeers ::
      !(Maybe [Text]),
    _vithStatus :: !(Maybe Text),
    _vithTestDurationInMinutes ::
      !(Maybe Int),
    _vithStartTime :: !(Maybe POSIX),
    _vithTestId :: !(Maybe Text),
    _vithEndTime :: !(Maybe POSIX),
    _vithOwnerAccount :: !(Maybe Text),
    _vithVirtualInterfaceId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VirtualInterfaceTestHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vithBgpPeers' - The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
--
-- * 'vithStatus' - The status of the virtual interface failover test.
--
-- * 'vithTestDurationInMinutes' - The time that the virtual interface failover test ran in minutes.
--
-- * 'vithStartTime' - The time that the virtual interface moves to the DOWN state.
--
-- * 'vithTestId' - The ID of the virtual interface failover test.
--
-- * 'vithEndTime' - The time that the virtual interface moves out of the DOWN state.
--
-- * 'vithOwnerAccount' - The owner ID of the tested virtual interface.
--
-- * 'vithVirtualInterfaceId' - The ID of the tested virtual interface.
virtualInterfaceTestHistory ::
  VirtualInterfaceTestHistory
virtualInterfaceTestHistory =
  VirtualInterfaceTestHistory'
    { _vithBgpPeers = Nothing,
      _vithStatus = Nothing,
      _vithTestDurationInMinutes = Nothing,
      _vithStartTime = Nothing,
      _vithTestId = Nothing,
      _vithEndTime = Nothing,
      _vithOwnerAccount = Nothing,
      _vithVirtualInterfaceId = Nothing
    }

-- | The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
vithBgpPeers :: Lens' VirtualInterfaceTestHistory [Text]
vithBgpPeers = lens _vithBgpPeers (\s a -> s {_vithBgpPeers = a}) . _Default . _Coerce

-- | The status of the virtual interface failover test.
vithStatus :: Lens' VirtualInterfaceTestHistory (Maybe Text)
vithStatus = lens _vithStatus (\s a -> s {_vithStatus = a})

-- | The time that the virtual interface failover test ran in minutes.
vithTestDurationInMinutes :: Lens' VirtualInterfaceTestHistory (Maybe Int)
vithTestDurationInMinutes = lens _vithTestDurationInMinutes (\s a -> s {_vithTestDurationInMinutes = a})

-- | The time that the virtual interface moves to the DOWN state.
vithStartTime :: Lens' VirtualInterfaceTestHistory (Maybe UTCTime)
vithStartTime = lens _vithStartTime (\s a -> s {_vithStartTime = a}) . mapping _Time

-- | The ID of the virtual interface failover test.
vithTestId :: Lens' VirtualInterfaceTestHistory (Maybe Text)
vithTestId = lens _vithTestId (\s a -> s {_vithTestId = a})

-- | The time that the virtual interface moves out of the DOWN state.
vithEndTime :: Lens' VirtualInterfaceTestHistory (Maybe UTCTime)
vithEndTime = lens _vithEndTime (\s a -> s {_vithEndTime = a}) . mapping _Time

-- | The owner ID of the tested virtual interface.
vithOwnerAccount :: Lens' VirtualInterfaceTestHistory (Maybe Text)
vithOwnerAccount = lens _vithOwnerAccount (\s a -> s {_vithOwnerAccount = a})

-- | The ID of the tested virtual interface.
vithVirtualInterfaceId :: Lens' VirtualInterfaceTestHistory (Maybe Text)
vithVirtualInterfaceId = lens _vithVirtualInterfaceId (\s a -> s {_vithVirtualInterfaceId = a})

instance FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            <$> (x .:? "bgpPeers" .!= mempty)
            <*> (x .:? "status")
            <*> (x .:? "testDurationInMinutes")
            <*> (x .:? "startTime")
            <*> (x .:? "testId")
            <*> (x .:? "endTime")
            <*> (x .:? "ownerAccount")
            <*> (x .:? "virtualInterfaceId")
      )

instance Hashable VirtualInterfaceTestHistory

instance NFData VirtualInterfaceTestHistory
