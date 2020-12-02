{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsert
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsert where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
import Network.AWS.Prelude

-- | Scte35 Splice Insert
--
-- /See:/ 'scte35SpliceInsert' smart constructor.
data Scte35SpliceInsert = Scte35SpliceInsert'
  { _ssiWebDeliveryAllowedFlag ::
      !(Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior),
    _ssiAdAvailOffset :: !(Maybe Int),
    _ssiNoRegionalBlackoutFlag ::
      !(Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35SpliceInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssiWebDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- * 'ssiAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- * 'ssiNoRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
scte35SpliceInsert ::
  Scte35SpliceInsert
scte35SpliceInsert =
  Scte35SpliceInsert'
    { _ssiWebDeliveryAllowedFlag = Nothing,
      _ssiAdAvailOffset = Nothing,
      _ssiNoRegionalBlackoutFlag = Nothing
    }

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
ssiWebDeliveryAllowedFlag :: Lens' Scte35SpliceInsert (Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior)
ssiWebDeliveryAllowedFlag = lens _ssiWebDeliveryAllowedFlag (\s a -> s {_ssiWebDeliveryAllowedFlag = a})

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
ssiAdAvailOffset :: Lens' Scte35SpliceInsert (Maybe Int)
ssiAdAvailOffset = lens _ssiAdAvailOffset (\s a -> s {_ssiAdAvailOffset = a})

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
ssiNoRegionalBlackoutFlag :: Lens' Scte35SpliceInsert (Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior)
ssiNoRegionalBlackoutFlag = lens _ssiNoRegionalBlackoutFlag (\s a -> s {_ssiNoRegionalBlackoutFlag = a})

instance FromJSON Scte35SpliceInsert where
  parseJSON =
    withObject
      "Scte35SpliceInsert"
      ( \x ->
          Scte35SpliceInsert'
            <$> (x .:? "webDeliveryAllowedFlag")
            <*> (x .:? "adAvailOffset")
            <*> (x .:? "noRegionalBlackoutFlag")
      )

instance Hashable Scte35SpliceInsert

instance NFData Scte35SpliceInsert

instance ToJSON Scte35SpliceInsert where
  toJSON Scte35SpliceInsert' {..} =
    object
      ( catMaybes
          [ ("webDeliveryAllowedFlag" .=) <$> _ssiWebDeliveryAllowedFlag,
            ("adAvailOffset" .=) <$> _ssiAdAvailOffset,
            ("noRegionalBlackoutFlag" .=) <$> _ssiNoRegionalBlackoutFlag
          ]
      )
