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
-- Module      : Network.AWS.Comprehend.DetectSyntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for syntax and the part of speech of words in the document. For more information, 'how-syntax' .
module Network.AWS.Comprehend.DetectSyntax
  ( -- * Creating a Request
    detectSyntax,
    DetectSyntax,

    -- * Request Lenses
    detText,
    detLanguageCode,

    -- * Destructuring the Response
    detectSyntaxResponse,
    DetectSyntaxResponse,

    -- * Response Lenses
    dsrsSyntaxTokens,
    dsrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detectSyntax' smart constructor.
data DetectSyntax = DetectSyntax'
  { _detText :: !(Sensitive Text),
    _detLanguageCode :: !SyntaxLanguageCode
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectSyntax' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detText' - A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
--
-- * 'detLanguageCode' - The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
detectSyntax ::
  -- | 'detText'
  Text ->
  -- | 'detLanguageCode'
  SyntaxLanguageCode ->
  DetectSyntax
detectSyntax pText_ pLanguageCode_ =
  DetectSyntax'
    { _detText = _Sensitive # pText_,
      _detLanguageCode = pLanguageCode_
    }

-- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF encoded characters.
detText :: Lens' DetectSyntax Text
detText = lens _detText (\s a -> s {_detText = a}) . _Sensitive

-- | The language code of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt").
detLanguageCode :: Lens' DetectSyntax SyntaxLanguageCode
detLanguageCode = lens _detLanguageCode (\s a -> s {_detLanguageCode = a})

instance AWSRequest DetectSyntax where
  type Rs DetectSyntax = DetectSyntaxResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DetectSyntaxResponse'
            <$> (x .?> "SyntaxTokens" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DetectSyntax

instance NFData DetectSyntax

instance ToHeaders DetectSyntax where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DetectSyntax" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetectSyntax where
  toJSON DetectSyntax' {..} =
    object
      ( catMaybes
          [ Just ("Text" .= _detText),
            Just ("LanguageCode" .= _detLanguageCode)
          ]
      )

instance ToPath DetectSyntax where
  toPath = const "/"

instance ToQuery DetectSyntax where
  toQuery = const mempty

-- | /See:/ 'detectSyntaxResponse' smart constructor.
data DetectSyntaxResponse = DetectSyntaxResponse'
  { _dsrsSyntaxTokens ::
      !(Maybe [SyntaxToken]),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetectSyntaxResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSyntaxTokens' - A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
--
-- * 'dsrsResponseStatus' - -- | The response status code.
detectSyntaxResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DetectSyntaxResponse
detectSyntaxResponse pResponseStatus_ =
  DetectSyntaxResponse'
    { _dsrsSyntaxTokens = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | A collection of syntax tokens describing the text. For each token, the response provides the text, the token type, where the text begins and ends, and the level of confidence that Amazon Comprehend has that the token is correct. For a list of token types, see 'how-syntax' .
dsrsSyntaxTokens :: Lens' DetectSyntaxResponse [SyntaxToken]
dsrsSyntaxTokens = lens _dsrsSyntaxTokens (\s a -> s {_dsrsSyntaxTokens = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DetectSyntaxResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DetectSyntaxResponse
