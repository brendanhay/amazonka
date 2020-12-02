{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectSentiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text and returns an inference of the prevailing sentiment (@POSITIVE@ , @NEUTRAL@ , @MIXED@ , or @NEGATIVE@ ).
module Network.AWS.Comprehend.DetectSentiment
  ( -- * Creating a Request
    detectSentiment,
    DetectSentiment,

    -- * Request Lenses
    dsText,
    dsLanguageCode,

    -- * Destructuring the Response
    detectSentimentResponse,
    DetectSentimentResponse,

    -- * Response Lenses
    detrsSentiment,
    detrsSentimentScore,
    detrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
  { _dsText ::
      !(Sensitive Text),
    _dsLanguageCode :: !LanguageCode
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectSentiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'dsLanguageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
detectSentiment ::
  -- | 'dsText'
  Text ->
  -- | 'dsLanguageCode'
  LanguageCode ->
  DetectSentiment
detectSentiment pText_ pLanguageCode_ =
  DetectSentiment'
    { _dsText = _Sensitive # pText_,
      _dsLanguageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
dsText :: Lens' DetectSentiment Text
dsText = lens _dsText (\s a -> s {_dsText = a}) . _Sensitive

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
dsLanguageCode :: Lens' DetectSentiment LanguageCode
dsLanguageCode = lens _dsLanguageCode (\s a -> s {_dsLanguageCode = a})

instance AWSRequest DetectSentiment where
  type Rs DetectSentiment = DetectSentimentResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DetectSentimentResponse'
            <$> (x .?> "Sentiment")
            <*> (x .?> "SentimentScore")
            <*> (pure (fromEnum s))
      )

instance Hashable DetectSentiment

instance NFData DetectSentiment

instance ToHeaders DetectSentiment where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DetectSentiment" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectSentiment where
  toJSON DetectSentiment' {..} =
    object
      ( catMaybes
          [ Just ("Text" .= _dsText),
            Just ("LanguageCode" .= _dsLanguageCode)
          ]
      )

instance ToPath DetectSentiment where
  toPath = const "/"

instance ToQuery DetectSentiment where
  toQuery = const mempty

-- | /See:/ 'detectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { _detrsSentiment ::
      !(Maybe SentimentType),
    _detrsSentimentScore ::
      !(Maybe SentimentScore),
    _detrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectSentimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsSentiment' - The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
--
-- * 'detrsSentimentScore' - An object that lists the sentiments, and their corresponding confidence levels.
--
-- * 'detrsResponseStatus' - -- | The response status code.
detectSentimentResponse ::
  -- | 'detrsResponseStatus'
  Int ->
  DetectSentimentResponse
detectSentimentResponse pResponseStatus_ =
  DetectSentimentResponse'
    { _detrsSentiment = Nothing,
      _detrsSentimentScore = Nothing,
      _detrsResponseStatus = pResponseStatus_
    }

-- | The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
detrsSentiment :: Lens' DetectSentimentResponse (Maybe SentimentType)
detrsSentiment = lens _detrsSentiment (\s a -> s {_detrsSentiment = a})

-- | An object that lists the sentiments, and their corresponding confidence levels.
detrsSentimentScore :: Lens' DetectSentimentResponse (Maybe SentimentScore)
detrsSentimentScore = lens _detrsSentimentScore (\s a -> s {_detrsSentimentScore = a})

-- | -- | The response status code.
detrsResponseStatus :: Lens' DetectSentimentResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\s a -> s {_detrsResponseStatus = a})

instance NFData DetectSentimentResponse
