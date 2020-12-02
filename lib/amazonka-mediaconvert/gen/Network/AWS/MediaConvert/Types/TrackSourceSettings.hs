{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TrackSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TrackSourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings specific to caption sources that are specified by track number. Currently, this is only IMSC captions in an IMF package. If your caption source is IMSC 1.1 in a separate xml file, use FileSourceSettings instead of TrackSourceSettings.
--
-- /See:/ 'trackSourceSettings' smart constructor.
newtype TrackSourceSettings = TrackSourceSettings'
  { _tssTrackNumber ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrackSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssTrackNumber' - Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
trackSourceSettings ::
  TrackSourceSettings
trackSourceSettings =
  TrackSourceSettings' {_tssTrackNumber = Nothing}

-- | Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
tssTrackNumber :: Lens' TrackSourceSettings (Maybe Natural)
tssTrackNumber = lens _tssTrackNumber (\s a -> s {_tssTrackNumber = a}) . mapping _Nat

instance FromJSON TrackSourceSettings where
  parseJSON =
    withObject
      "TrackSourceSettings"
      (\x -> TrackSourceSettings' <$> (x .:? "trackNumber"))

instance Hashable TrackSourceSettings

instance NFData TrackSourceSettings

instance ToJSON TrackSourceSettings where
  toJSON TrackSourceSettings' {..} =
    object (catMaybes [("trackNumber" .=) <$> _tssTrackNumber])
