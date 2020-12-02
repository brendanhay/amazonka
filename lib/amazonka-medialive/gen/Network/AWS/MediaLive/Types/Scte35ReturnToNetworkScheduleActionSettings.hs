{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for a SCTE-35 return_to_network message.
--
-- /See:/ 'scte35ReturnToNetworkScheduleActionSettings' smart constructor.
newtype Scte35ReturnToNetworkScheduleActionSettings = Scte35ReturnToNetworkScheduleActionSettings'
  { _srtnsasSpliceEventId ::
      Nat
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'Scte35ReturnToNetworkScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtnsasSpliceEventId' - The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
scte35ReturnToNetworkScheduleActionSettings ::
  -- | 'srtnsasSpliceEventId'
  Natural ->
  Scte35ReturnToNetworkScheduleActionSettings
scte35ReturnToNetworkScheduleActionSettings pSpliceEventId_ =
  Scte35ReturnToNetworkScheduleActionSettings'
    { _srtnsasSpliceEventId =
        _Nat # pSpliceEventId_
    }

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
srtnsasSpliceEventId :: Lens' Scte35ReturnToNetworkScheduleActionSettings Natural
srtnsasSpliceEventId = lens _srtnsasSpliceEventId (\s a -> s {_srtnsasSpliceEventId = a}) . _Nat

instance FromJSON Scte35ReturnToNetworkScheduleActionSettings where
  parseJSON =
    withObject
      "Scte35ReturnToNetworkScheduleActionSettings"
      ( \x ->
          Scte35ReturnToNetworkScheduleActionSettings'
            <$> (x .: "spliceEventId")
      )

instance Hashable Scte35ReturnToNetworkScheduleActionSettings

instance NFData Scte35ReturnToNetworkScheduleActionSettings

instance ToJSON Scte35ReturnToNetworkScheduleActionSettings where
  toJSON Scte35ReturnToNetworkScheduleActionSettings' {..} =
    object
      (catMaybes [Just ("spliceEventId" .= _srtnsasSpliceEventId)])
