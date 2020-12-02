{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityRecognition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.CelebrityDetail

-- | Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
--
--
-- /See:/ 'celebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { _crCelebrity ::
      !(Maybe CelebrityDetail),
    _crTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CelebrityRecognition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crCelebrity' - Information about a recognized celebrity.
--
-- * 'crTimestamp' - The time, in milliseconds from the start of the video, that the celebrity was recognized.
celebrityRecognition ::
  CelebrityRecognition
celebrityRecognition =
  CelebrityRecognition'
    { _crCelebrity = Nothing,
      _crTimestamp = Nothing
    }

-- | Information about a recognized celebrity.
crCelebrity :: Lens' CelebrityRecognition (Maybe CelebrityDetail)
crCelebrity = lens _crCelebrity (\s a -> s {_crCelebrity = a})

-- | The time, in milliseconds from the start of the video, that the celebrity was recognized.
crTimestamp :: Lens' CelebrityRecognition (Maybe Integer)
crTimestamp = lens _crTimestamp (\s a -> s {_crTimestamp = a})

instance FromJSON CelebrityRecognition where
  parseJSON =
    withObject
      "CelebrityRecognition"
      ( \x ->
          CelebrityRecognition'
            <$> (x .:? "Celebrity") <*> (x .:? "Timestamp")
      )

instance Hashable CelebrityRecognition

instance NFData CelebrityRecognition
