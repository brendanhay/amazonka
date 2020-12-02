{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.LabelDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.LabelDetection where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Label

-- | Information about a label detected in a video analysis request and the time the label was detected in the video.
--
--
--
-- /See:/ 'labelDetection' smart constructor.
data LabelDetection = LabelDetection'
  { _ldLabel :: !(Maybe Label),
    _ldTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelDetection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldLabel' - Details about the detected label.
--
-- * 'ldTimestamp' - Time, in milliseconds from the start of the video, that the label was detected.
labelDetection ::
  LabelDetection
labelDetection =
  LabelDetection' {_ldLabel = Nothing, _ldTimestamp = Nothing}

-- | Details about the detected label.
ldLabel :: Lens' LabelDetection (Maybe Label)
ldLabel = lens _ldLabel (\s a -> s {_ldLabel = a})

-- | Time, in milliseconds from the start of the video, that the label was detected.
ldTimestamp :: Lens' LabelDetection (Maybe Integer)
ldTimestamp = lens _ldTimestamp (\s a -> s {_ldTimestamp = a})

instance FromJSON LabelDetection where
  parseJSON =
    withObject
      "LabelDetection"
      ( \x ->
          LabelDetection' <$> (x .:? "Label") <*> (x .:? "Timestamp")
      )

instance Hashable LabelDetection

instance NFData LabelDetection
