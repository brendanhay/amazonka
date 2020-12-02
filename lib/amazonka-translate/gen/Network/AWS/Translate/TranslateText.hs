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
-- Module      : Network.AWS.Translate.TranslateText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates input text from the source language to the target language. For a list of available languages and language codes, see 'what-is-languages' .
module Network.AWS.Translate.TranslateText
  ( -- * Creating a Request
    translateText,
    TranslateText,

    -- * Request Lenses
    ttTerminologyNames,
    ttText,
    ttSourceLanguageCode,
    ttTargetLanguageCode,

    -- * Destructuring the Response
    translateTextResponse,
    TranslateTextResponse,

    -- * Response Lenses
    ttrsAppliedTerminologies,
    ttrsResponseStatus,
    ttrsTranslatedText,
    ttrsSourceLanguageCode,
    ttrsTargetLanguageCode,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'translateText' smart constructor.
data TranslateText = TranslateText'
  { _ttTerminologyNames ::
      !(Maybe [Text]),
    _ttText :: !Text,
    _ttSourceLanguageCode :: !Text,
    _ttTargetLanguageCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TranslateText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttTerminologyNames' - The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
--
-- * 'ttText' - The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
--
-- * 'ttSourceLanguageCode' - The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' . To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
--
-- * 'ttTargetLanguageCode' - The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
translateText ::
  -- | 'ttText'
  Text ->
  -- | 'ttSourceLanguageCode'
  Text ->
  -- | 'ttTargetLanguageCode'
  Text ->
  TranslateText
translateText pText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateText'
    { _ttTerminologyNames = Nothing,
      _ttText = pText_,
      _ttSourceLanguageCode = pSourceLanguageCode_,
      _ttTargetLanguageCode = pTargetLanguageCode_
    }

-- | The name of the terminology list file to be used in the TranslateText request. You can use 1 terminology list at most in a @TranslateText@ request. Terminology lists can contain a maximum of 256 terms.
ttTerminologyNames :: Lens' TranslateText [Text]
ttTerminologyNames = lens _ttTerminologyNames (\s a -> s {_ttTerminologyNames = a}) . _Default . _Coerce

-- | The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
ttText :: Lens' TranslateText Text
ttText = lens _ttText (\s a -> s {_ttText = a})

-- | The language code for the language of the source text. The language must be a language supported by Amazon Translate. For a list of language codes, see 'what-is-languages' . To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend> to determine the source language.
ttSourceLanguageCode :: Lens' TranslateText Text
ttSourceLanguageCode = lens _ttSourceLanguageCode (\s a -> s {_ttSourceLanguageCode = a})

-- | The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
ttTargetLanguageCode :: Lens' TranslateText Text
ttTargetLanguageCode = lens _ttTargetLanguageCode (\s a -> s {_ttTargetLanguageCode = a})

instance AWSRequest TranslateText where
  type Rs TranslateText = TranslateTextResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          TranslateTextResponse'
            <$> (x .?> "AppliedTerminologies" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "TranslatedText")
            <*> (x .:> "SourceLanguageCode")
            <*> (x .:> "TargetLanguageCode")
      )

instance Hashable TranslateText

instance NFData TranslateText

instance ToHeaders TranslateText where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShineFrontendService_20170701.TranslateText" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TranslateText where
  toJSON TranslateText' {..} =
    object
      ( catMaybes
          [ ("TerminologyNames" .=) <$> _ttTerminologyNames,
            Just ("Text" .= _ttText),
            Just ("SourceLanguageCode" .= _ttSourceLanguageCode),
            Just ("TargetLanguageCode" .= _ttTargetLanguageCode)
          ]
      )

instance ToPath TranslateText where
  toPath = const "/"

instance ToQuery TranslateText where
  toQuery = const mempty

-- | /See:/ 'translateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { _ttrsAppliedTerminologies ::
      !(Maybe [AppliedTerminology]),
    _ttrsResponseStatus :: !Int,
    _ttrsTranslatedText :: !Text,
    _ttrsSourceLanguageCode :: !Text,
    _ttrsTargetLanguageCode :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TranslateTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrsAppliedTerminologies' - The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
--
-- * 'ttrsResponseStatus' - -- | The response status code.
--
-- * 'ttrsTranslatedText' - The translated text.
--
-- * 'ttrsSourceLanguageCode' - The language code for the language of the source text.
--
-- * 'ttrsTargetLanguageCode' - The language code for the language of the target text.
translateTextResponse ::
  -- | 'ttrsResponseStatus'
  Int ->
  -- | 'ttrsTranslatedText'
  Text ->
  -- | 'ttrsSourceLanguageCode'
  Text ->
  -- | 'ttrsTargetLanguageCode'
  Text ->
  TranslateTextResponse
translateTextResponse
  pResponseStatus_
  pTranslatedText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateTextResponse'
      { _ttrsAppliedTerminologies = Nothing,
        _ttrsResponseStatus = pResponseStatus_,
        _ttrsTranslatedText = pTranslatedText_,
        _ttrsSourceLanguageCode = pSourceLanguageCode_,
        _ttrsTargetLanguageCode = pTargetLanguageCode_
      }

-- | The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
ttrsAppliedTerminologies :: Lens' TranslateTextResponse [AppliedTerminology]
ttrsAppliedTerminologies = lens _ttrsAppliedTerminologies (\s a -> s {_ttrsAppliedTerminologies = a}) . _Default . _Coerce

-- | -- | The response status code.
ttrsResponseStatus :: Lens' TranslateTextResponse Int
ttrsResponseStatus = lens _ttrsResponseStatus (\s a -> s {_ttrsResponseStatus = a})

-- | The translated text.
ttrsTranslatedText :: Lens' TranslateTextResponse Text
ttrsTranslatedText = lens _ttrsTranslatedText (\s a -> s {_ttrsTranslatedText = a})

-- | The language code for the language of the source text.
ttrsSourceLanguageCode :: Lens' TranslateTextResponse Text
ttrsSourceLanguageCode = lens _ttrsSourceLanguageCode (\s a -> s {_ttrsSourceLanguageCode = a})

-- | The language code for the language of the target text.
ttrsTargetLanguageCode :: Lens' TranslateTextResponse Text
ttrsTargetLanguageCode = lens _ttrsTargetLanguageCode (\s a -> s {_ttrsTargetLanguageCode = a})

instance NFData TranslateTextResponse
