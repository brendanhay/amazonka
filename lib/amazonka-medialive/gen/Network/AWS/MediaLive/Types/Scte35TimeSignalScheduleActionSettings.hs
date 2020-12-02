{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35Descriptor
import Network.AWS.Prelude

-- | Settings for a SCTE-35 time_signal.
--
-- /See:/ 'scte35TimeSignalScheduleActionSettings' smart constructor.
newtype Scte35TimeSignalScheduleActionSettings = Scte35TimeSignalScheduleActionSettings'
  { _stssasScte35Descriptors ::
      [Scte35Descriptor]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'Scte35TimeSignalScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stssasScte35Descriptors' - The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
scte35TimeSignalScheduleActionSettings ::
  Scte35TimeSignalScheduleActionSettings
scte35TimeSignalScheduleActionSettings =
  Scte35TimeSignalScheduleActionSettings'
    { _stssasScte35Descriptors =
        mempty
    }

-- | The list of SCTE-35 descriptors accompanying the SCTE-35 time_signal.
stssasScte35Descriptors :: Lens' Scte35TimeSignalScheduleActionSettings [Scte35Descriptor]
stssasScte35Descriptors = lens _stssasScte35Descriptors (\s a -> s {_stssasScte35Descriptors = a}) . _Coerce

instance FromJSON Scte35TimeSignalScheduleActionSettings where
  parseJSON =
    withObject
      "Scte35TimeSignalScheduleActionSettings"
      ( \x ->
          Scte35TimeSignalScheduleActionSettings'
            <$> (x .:? "scte35Descriptors" .!= mempty)
      )

instance Hashable Scte35TimeSignalScheduleActionSettings

instance NFData Scte35TimeSignalScheduleActionSettings

instance ToJSON Scte35TimeSignalScheduleActionSettings where
  toJSON Scte35TimeSignalScheduleActionSettings' {..} =
    object
      ( catMaybes
          [Just ("scte35Descriptors" .= _stssasScte35Descriptors)]
      )
