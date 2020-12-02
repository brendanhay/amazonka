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
-- Module      : Network.AWS.Comprehend.DetectKeyPhrases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in the text.
module Network.AWS.Comprehend.DetectKeyPhrases
  ( -- * Creating a Request
    detectKeyPhrases,
    DetectKeyPhrases,

    -- * Request Lenses
    dkpText,
    dkpLanguageCode,

    -- * Destructuring the Response
    detectKeyPhrasesResponse,
    DetectKeyPhrasesResponse,

    -- * Response Lenses
    dkprsKeyPhrases,
    dkprsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { _dkpText ::
      !(Sensitive Text),
    _dkpLanguageCode :: !LanguageCode
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectKeyPhrases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkpText' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- * 'dkpLanguageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
detectKeyPhrases ::
  -- | 'dkpText'
  Text ->
  -- | 'dkpLanguageCode'
  LanguageCode ->
  DetectKeyPhrases
detectKeyPhrases pText_ pLanguageCode_ =
  DetectKeyPhrases'
    { _dkpText = _Sensitive # pText_,
      _dkpLanguageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of UTF-8 encoded characters.
dkpText :: Lens' DetectKeyPhrases Text
dkpText = lens _dkpText (\s a -> s {_dkpText = a}) . _Sensitive

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
dkpLanguageCode :: Lens' DetectKeyPhrases LanguageCode
dkpLanguageCode = lens _dkpLanguageCode (\s a -> s {_dkpLanguageCode = a})

instance AWSRequest DetectKeyPhrases where
  type Rs DetectKeyPhrases = DetectKeyPhrasesResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DetectKeyPhrasesResponse'
            <$> (x .?> "KeyPhrases" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DetectKeyPhrases

instance NFData DetectKeyPhrases

instance ToHeaders DetectKeyPhrases where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DetectKeyPhrases" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectKeyPhrases where
  toJSON DetectKeyPhrases' {..} =
    object
      ( catMaybes
          [ Just ("Text" .= _dkpText),
            Just ("LanguageCode" .= _dkpLanguageCode)
          ]
      )

instance ToPath DetectKeyPhrases where
  toPath = const "/"

instance ToQuery DetectKeyPhrases where
  toQuery = const mempty

-- | /See:/ 'detectKeyPhrasesResponse' smart constructor.
data DetectKeyPhrasesResponse = DetectKeyPhrasesResponse'
  { _dkprsKeyPhrases ::
      !(Maybe [KeyPhrase]),
    _dkprsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectKeyPhrasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkprsKeyPhrases' - A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- * 'dkprsResponseStatus' - -- | The response status code.
detectKeyPhrasesResponse ::
  -- | 'dkprsResponseStatus'
  Int ->
  DetectKeyPhrasesResponse
detectKeyPhrasesResponse pResponseStatus_ =
  DetectKeyPhrasesResponse'
    { _dkprsKeyPhrases = Nothing,
      _dkprsResponseStatus = pResponseStatus_
    }

-- | A collection of key phrases that Amazon Comprehend identified in the input text. For each key phrase, the response provides the text of the key phrase, where the key phrase begins and ends, and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
dkprsKeyPhrases :: Lens' DetectKeyPhrasesResponse [KeyPhrase]
dkprsKeyPhrases = lens _dkprsKeyPhrases (\s a -> s {_dkprsKeyPhrases = a}) . _Default . _Coerce

-- | -- | The response status code.
dkprsResponseStatus :: Lens' DetectKeyPhrasesResponse Int
dkprsResponseStatus = lens _dkprsResponseStatus (\s a -> s {_dkprsResponseStatus = a})

instance NFData DetectKeyPhrasesResponse
