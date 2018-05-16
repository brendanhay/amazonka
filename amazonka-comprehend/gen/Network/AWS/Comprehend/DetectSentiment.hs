{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DetectSentiment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text and returns an inference of the prevailing sentiment (@POSITIVE@ , @NEUTRAL@ , @MIXED@ , or @NEGATIVE@ ).
--
--
module Network.AWS.Comprehend.DetectSentiment
    (
    -- * Creating a Request
      detectSentiment
    , DetectSentiment
    -- * Request Lenses
    , dsText
    , dsLanguageCode

    -- * Destructuring the Response
    , detectSentimentResponse
    , DetectSentimentResponse
    -- * Response Lenses
    , dsrsSentiment
    , dsrsSentimentScore
    , dsrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
  { _dsText         :: !Text
  , _dsLanguageCode :: !LanguageCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectSentiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'dsLanguageCode' - The RFC 5646 language code for the input text. If you don't specify a language code, Amazon Comprehend detects the dominant language. If you specify the code for a language that Amazon Comprehend does not support, it returns and @UnsupportedLanguageException@ . For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
detectSentiment
    :: Text -- ^ 'dsText'
    -> LanguageCode -- ^ 'dsLanguageCode'
    -> DetectSentiment
detectSentiment pText_ pLanguageCode_ =
  DetectSentiment' {_dsText = pText_, _dsLanguageCode = pLanguageCode_}


-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
dsText :: Lens' DetectSentiment Text
dsText = lens _dsText (\ s a -> s{_dsText = a})

-- | The RFC 5646 language code for the input text. If you don't specify a language code, Amazon Comprehend detects the dominant language. If you specify the code for a language that Amazon Comprehend does not support, it returns and @UnsupportedLanguageException@ . For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
dsLanguageCode :: Lens' DetectSentiment LanguageCode
dsLanguageCode = lens _dsLanguageCode (\ s a -> s{_dsLanguageCode = a})

instance AWSRequest DetectSentiment where
        type Rs DetectSentiment = DetectSentimentResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 DetectSentimentResponse' <$>
                   (x .?> "Sentiment") <*> (x .?> "SentimentScore") <*>
                     (pure (fromEnum s)))

instance Hashable DetectSentiment where

instance NFData DetectSentiment where

instance ToHeaders DetectSentiment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.DetectSentiment" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetectSentiment where
        toJSON DetectSentiment'{..}
          = object
              (catMaybes
                 [Just ("Text" .= _dsText),
                  Just ("LanguageCode" .= _dsLanguageCode)])

instance ToPath DetectSentiment where
        toPath = const "/"

instance ToQuery DetectSentiment where
        toQuery = const mempty

-- | /See:/ 'detectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { _dsrsSentiment      :: !(Maybe SentimentType)
  , _dsrsSentimentScore :: !(Maybe SentimentScore)
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectSentimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSentiment' - The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
--
-- * 'dsrsSentimentScore' - An object that lists the sentiments, and their corresponding confidence levels.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
detectSentimentResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DetectSentimentResponse
detectSentimentResponse pResponseStatus_ =
  DetectSentimentResponse'
    { _dsrsSentiment = Nothing
    , _dsrsSentimentScore = Nothing
    , _dsrsResponseStatus = pResponseStatus_
    }


-- | The inferred sentiment that Amazon Comprehend has the highest level of confidence in.
dsrsSentiment :: Lens' DetectSentimentResponse (Maybe SentimentType)
dsrsSentiment = lens _dsrsSentiment (\ s a -> s{_dsrsSentiment = a})

-- | An object that lists the sentiments, and their corresponding confidence levels.
dsrsSentimentScore :: Lens' DetectSentimentResponse (Maybe SentimentScore)
dsrsSentimentScore = lens _dsrsSentimentScore (\ s a -> s{_dsrsSentimentScore = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DetectSentimentResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DetectSentimentResponse where
