{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Emotion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Emotion where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.EmotionName

-- | The emotions that appear to be expressed on the face, and the confidence level in the determination. The API is only making a determination of the physical appearance of a person's face. It is not a determination of the person’s internal emotional state and should not be used in such a way. For example, a person pretending to have a sad face might not be sad emotionally.
--
--
--
-- /See:/ 'emotion' smart constructor.
data Emotion = Emotion'
  { _eConfidence :: !(Maybe Double),
    _eType :: !(Maybe EmotionName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Emotion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eConfidence' - Level of confidence in the determination.
--
-- * 'eType' - Type of emotion detected.
emotion ::
  Emotion
emotion = Emotion' {_eConfidence = Nothing, _eType = Nothing}

-- | Level of confidence in the determination.
eConfidence :: Lens' Emotion (Maybe Double)
eConfidence = lens _eConfidence (\s a -> s {_eConfidence = a})

-- | Type of emotion detected.
eType :: Lens' Emotion (Maybe EmotionName)
eType = lens _eType (\s a -> s {_eType = a})

instance FromJSON Emotion where
  parseJSON =
    withObject
      "Emotion"
      (\x -> Emotion' <$> (x .:? "Confidence") <*> (x .:? "Type"))

instance Hashable Emotion

instance NFData Emotion
