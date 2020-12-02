{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalApos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalApos where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import Network.AWS.Prelude

-- | Scte35 Time Signal Apos
--
-- /See:/ 'scte35TimeSignalApos' smart constructor.
data Scte35TimeSignalApos = Scte35TimeSignalApos'
  { _stsaWebDeliveryAllowedFlag ::
      !(Maybe Scte35AposWebDeliveryAllowedBehavior),
    _stsaAdAvailOffset :: !(Maybe Int),
    _stsaNoRegionalBlackoutFlag ::
      !(Maybe Scte35AposNoRegionalBlackoutBehavior)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35TimeSignalApos' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsaWebDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- * 'stsaAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- * 'stsaNoRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
scte35TimeSignalApos ::
  Scte35TimeSignalApos
scte35TimeSignalApos =
  Scte35TimeSignalApos'
    { _stsaWebDeliveryAllowedFlag = Nothing,
      _stsaAdAvailOffset = Nothing,
      _stsaNoRegionalBlackoutFlag = Nothing
    }

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
stsaWebDeliveryAllowedFlag :: Lens' Scte35TimeSignalApos (Maybe Scte35AposWebDeliveryAllowedBehavior)
stsaWebDeliveryAllowedFlag = lens _stsaWebDeliveryAllowedFlag (\s a -> s {_stsaWebDeliveryAllowedFlag = a})

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
stsaAdAvailOffset :: Lens' Scte35TimeSignalApos (Maybe Int)
stsaAdAvailOffset = lens _stsaAdAvailOffset (\s a -> s {_stsaAdAvailOffset = a})

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
stsaNoRegionalBlackoutFlag :: Lens' Scte35TimeSignalApos (Maybe Scte35AposNoRegionalBlackoutBehavior)
stsaNoRegionalBlackoutFlag = lens _stsaNoRegionalBlackoutFlag (\s a -> s {_stsaNoRegionalBlackoutFlag = a})

instance FromJSON Scte35TimeSignalApos where
  parseJSON =
    withObject
      "Scte35TimeSignalApos"
      ( \x ->
          Scte35TimeSignalApos'
            <$> (x .:? "webDeliveryAllowedFlag")
            <*> (x .:? "adAvailOffset")
            <*> (x .:? "noRegionalBlackoutFlag")
      )

instance Hashable Scte35TimeSignalApos

instance NFData Scte35TimeSignalApos

instance ToJSON Scte35TimeSignalApos where
  toJSON Scte35TimeSignalApos' {..} =
    object
      ( catMaybes
          [ ("webDeliveryAllowedFlag" .=) <$> _stsaWebDeliveryAllowedFlag,
            ("adAvailOffset" .=) <$> _stsaAdAvailOffset,
            ("noRegionalBlackoutFlag" .=) <$> _stsaNoRegionalBlackoutFlag
          ]
      )
