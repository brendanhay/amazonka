{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentModerationDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentModerationDetection where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ModerationLabel

-- | Information about an unsafe content label detection in a stored video.
--
--
--
-- /See:/ 'contentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { _cmdModerationLabel ::
      !(Maybe ModerationLabel),
    _cmdTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContentModerationDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdModerationLabel' - The unsafe content label detected by in the stored video.
--
-- * 'cmdTimestamp' - Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
contentModerationDetection ::
  ContentModerationDetection
contentModerationDetection =
  ContentModerationDetection'
    { _cmdModerationLabel = Nothing,
      _cmdTimestamp = Nothing
    }

-- | The unsafe content label detected by in the stored video.
cmdModerationLabel :: Lens' ContentModerationDetection (Maybe ModerationLabel)
cmdModerationLabel = lens _cmdModerationLabel (\s a -> s {_cmdModerationLabel = a})

-- | Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
cmdTimestamp :: Lens' ContentModerationDetection (Maybe Integer)
cmdTimestamp = lens _cmdTimestamp (\s a -> s {_cmdTimestamp = a})

instance FromJSON ContentModerationDetection where
  parseJSON =
    withObject
      "ContentModerationDetection"
      ( \x ->
          ContentModerationDetection'
            <$> (x .:? "ModerationLabel") <*> (x .:? "Timestamp")
      )

instance Hashable ContentModerationDetection

instance NFData ContentModerationDetection
