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
-- Module      : Network.AWS.Translate.TranslateText
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates input text from the source language to the target language. You can translate between English (en) and one of the following languages, or between one of the following languages and English.
--
--
--     * Arabic (ar)
--
--     * Chinese (Simplified) (zh)
--
--     * French (fr)
--
--     * German (de)
--
--     * Portuguese (pt)
--
--     * Spanish (es)
--
--
--
-- To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call Amazon Comprehend to determine the source language.
--
module Network.AWS.Translate.TranslateText
    (
    -- * Creating a Request
      translateText
    , TranslateText
    -- * Request Lenses
    , ttText
    , ttSourceLanguageCode
    , ttTargetLanguageCode

    -- * Destructuring the Response
    , translateTextResponse
    , TranslateTextResponse
    -- * Response Lenses
    , ttrsResponseStatus
    , ttrsTranslatedText
    , ttrsSourceLanguageCode
    , ttrsTargetLanguageCode
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types
import Network.AWS.Translate.Types.Product

-- | /See:/ 'translateText' smart constructor.
data TranslateText = TranslateText'
  { _ttText               :: !Text
  , _ttSourceLanguageCode :: !Text
  , _ttTargetLanguageCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranslateText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttText' - The text to translate.
--
-- * 'ttSourceLanguageCode' - One of the supported language codes for the source text. If the @TargetLanguageCode@ is not "en", the @SourceLanguageCode@ must be "en". To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call Amazon Comprehend to determine the source language.
--
-- * 'ttTargetLanguageCode' - One of the supported language codes for the target text. If the @SourceLanguageCode@ is not "en", the @TargetLanguageCode@ must be "en".
translateText
    :: Text -- ^ 'ttText'
    -> Text -- ^ 'ttSourceLanguageCode'
    -> Text -- ^ 'ttTargetLanguageCode'
    -> TranslateText
translateText pText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateText'
    { _ttText = pText_
    , _ttSourceLanguageCode = pSourceLanguageCode_
    , _ttTargetLanguageCode = pTargetLanguageCode_
    }


-- | The text to translate.
ttText :: Lens' TranslateText Text
ttText = lens _ttText (\ s a -> s{_ttText = a})

-- | One of the supported language codes for the source text. If the @TargetLanguageCode@ is not "en", the @SourceLanguageCode@ must be "en". To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call Amazon Comprehend to determine the source language.
ttSourceLanguageCode :: Lens' TranslateText Text
ttSourceLanguageCode = lens _ttSourceLanguageCode (\ s a -> s{_ttSourceLanguageCode = a})

-- | One of the supported language codes for the target text. If the @SourceLanguageCode@ is not "en", the @TargetLanguageCode@ must be "en".
ttTargetLanguageCode :: Lens' TranslateText Text
ttTargetLanguageCode = lens _ttTargetLanguageCode (\ s a -> s{_ttTargetLanguageCode = a})

instance AWSRequest TranslateText where
        type Rs TranslateText = TranslateTextResponse
        request = postJSON translate
        response
          = receiveJSON
              (\ s h x ->
                 TranslateTextResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "TranslatedText") <*>
                     (x .:> "SourceLanguageCode")
                     <*> (x .:> "TargetLanguageCode"))

instance Hashable TranslateText where

instance NFData TranslateText where

instance ToHeaders TranslateText where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShineFrontendService_20170701.TranslateText" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TranslateText where
        toJSON TranslateText'{..}
          = object
              (catMaybes
                 [Just ("Text" .= _ttText),
                  Just ("SourceLanguageCode" .= _ttSourceLanguageCode),
                  Just
                    ("TargetLanguageCode" .= _ttTargetLanguageCode)])

instance ToPath TranslateText where
        toPath = const "/"

instance ToQuery TranslateText where
        toQuery = const mempty

-- | /See:/ 'translateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { _ttrsResponseStatus     :: !Int
  , _ttrsTranslatedText     :: !Text
  , _ttrsSourceLanguageCode :: !Text
  , _ttrsTargetLanguageCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranslateTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrsResponseStatus' - -- | The response status code.
--
-- * 'ttrsTranslatedText' - The text translated into the target language.
--
-- * 'ttrsSourceLanguageCode' - The language code for the language of the input text.
--
-- * 'ttrsTargetLanguageCode' - The language code for the language of the translated text.
translateTextResponse
    :: Int -- ^ 'ttrsResponseStatus'
    -> Text -- ^ 'ttrsTranslatedText'
    -> Text -- ^ 'ttrsSourceLanguageCode'
    -> Text -- ^ 'ttrsTargetLanguageCode'
    -> TranslateTextResponse
translateTextResponse pResponseStatus_ pTranslatedText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateTextResponse'
    { _ttrsResponseStatus = pResponseStatus_
    , _ttrsTranslatedText = pTranslatedText_
    , _ttrsSourceLanguageCode = pSourceLanguageCode_
    , _ttrsTargetLanguageCode = pTargetLanguageCode_
    }


-- | -- | The response status code.
ttrsResponseStatus :: Lens' TranslateTextResponse Int
ttrsResponseStatus = lens _ttrsResponseStatus (\ s a -> s{_ttrsResponseStatus = a})

-- | The text translated into the target language.
ttrsTranslatedText :: Lens' TranslateTextResponse Text
ttrsTranslatedText = lens _ttrsTranslatedText (\ s a -> s{_ttrsTranslatedText = a})

-- | The language code for the language of the input text.
ttrsSourceLanguageCode :: Lens' TranslateTextResponse Text
ttrsSourceLanguageCode = lens _ttrsSourceLanguageCode (\ s a -> s{_ttrsSourceLanguageCode = a})

-- | The language code for the language of the translated text.
ttrsTargetLanguageCode :: Lens' TranslateTextResponse Text
ttrsTargetLanguageCode = lens _ttrsTargetLanguageCode (\ s a -> s{_ttrsTargetLanguageCode = a})

instance NFData TranslateTextResponse where
