{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult where

import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SentimentType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
--
--
-- /See:/ 'batchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { _bSentiment ::
      !(Maybe SentimentType),
    _bSentimentScore ::
      !(Maybe SentimentScore),
    _bIndex :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetectSentimentItemResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bSentiment' - The sentiment detected in the document.
--
-- * 'bSentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
--
-- * 'bIndex' - The zero-based index of the document in the input list.
batchDetectSentimentItemResult ::
  BatchDetectSentimentItemResult
batchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { _bSentiment = Nothing,
      _bSentimentScore = Nothing,
      _bIndex = Nothing
    }

-- | The sentiment detected in the document.
bSentiment :: Lens' BatchDetectSentimentItemResult (Maybe SentimentType)
bSentiment = lens _bSentiment (\s a -> s {_bSentiment = a})

-- | The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
bSentimentScore :: Lens' BatchDetectSentimentItemResult (Maybe SentimentScore)
bSentimentScore = lens _bSentimentScore (\s a -> s {_bSentimentScore = a})

-- | The zero-based index of the document in the input list.
bIndex :: Lens' BatchDetectSentimentItemResult (Maybe Int)
bIndex = lens _bIndex (\s a -> s {_bIndex = a})

instance FromJSON BatchDetectSentimentItemResult where
  parseJSON =
    withObject
      "BatchDetectSentimentItemResult"
      ( \x ->
          BatchDetectSentimentItemResult'
            <$> (x .:? "Sentiment")
            <*> (x .:? "SentimentScore")
            <*> (x .:? "Index")
      )

instance Hashable BatchDetectSentimentItemResult

instance NFData BatchDetectSentimentItemResult
