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
-- Translates input text from the source language to the target language. It is not necessary to use English (en) as either the source or the target language but not all language combinations are supported by Amazon Translate. For more information, see <http://docs.aws.amazon.com/translate/latest/dg/pairs.html Supported Language Pairs> .
--
--
--     * Arabic (ar)
--
--     * Chinese (Simplified) (zh)
--
--     * Chinese (Traditional) (zh-TW)
--
--     * Czech (cs)
--
--     * Danish (da)
--
--     * Dutch (nl)
--
--     * English (en)
--
--     * Finnish (fi)
--
--     * French (fr)
--
--     * German (de)
--
--     * Hebrew (he)
--
--     * Indonesian (id)
--
--     * Italian (it)
--
--     * Japanese (ja)
--
--     * Korean (ko)
--
--     * Polish (pl)
--
--     * Portuguese (pt)
--
--     * Russian (ru)
--
--     * Spanish (es)
--
--     * Swedish (sv)
--
--     * Turkish (tr)
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
    , ttTerminologyNames
    , ttText
    , ttSourceLanguageCode
    , ttTargetLanguageCode

    -- * Destructuring the Response
    , translateTextResponse
    , TranslateTextResponse
    -- * Response Lenses
    , ttrsAppliedTerminologies
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
  { _ttTerminologyNames   :: !(Maybe [Text])
  , _ttText               :: !Text
  , _ttSourceLanguageCode :: !Text
  , _ttTargetLanguageCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranslateText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttTerminologyNames' - The TerminologyNames list that is taken as input to the TranslateText request. This has a minimum length of 0 and a maximum length of 1.
--
-- * 'ttText' - The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
--
-- * 'ttSourceLanguageCode' - The language code for the language of the source text. The language must be a language supported by Amazon Translate.  To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call Amazon Comprehend to determine the source language.
--
-- * 'ttTargetLanguageCode' - The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
translateText
    :: Text -- ^ 'ttText'
    -> Text -- ^ 'ttSourceLanguageCode'
    -> Text -- ^ 'ttTargetLanguageCode'
    -> TranslateText
translateText pText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateText'
    { _ttTerminologyNames = Nothing
    , _ttText = pText_
    , _ttSourceLanguageCode = pSourceLanguageCode_
    , _ttTargetLanguageCode = pTargetLanguageCode_
    }


-- | The TerminologyNames list that is taken as input to the TranslateText request. This has a minimum length of 0 and a maximum length of 1.
ttTerminologyNames :: Lens' TranslateText [Text]
ttTerminologyNames = lens _ttTerminologyNames (\ s a -> s{_ttTerminologyNames = a}) . _Default . _Coerce

-- | The text to translate. The text string can be a maximum of 5,000 bytes long. Depending on your character set, this may be fewer than 5,000 characters.
ttText :: Lens' TranslateText Text
ttText = lens _ttText (\ s a -> s{_ttText = a})

-- | The language code for the language of the source text. The language must be a language supported by Amazon Translate.  To have Amazon Translate determine the source language of your text, you can specify @auto@ in the @SourceLanguageCode@ field. If you specify @auto@ , Amazon Translate will call Amazon Comprehend to determine the source language.
ttSourceLanguageCode :: Lens' TranslateText Text
ttSourceLanguageCode = lens _ttSourceLanguageCode (\ s a -> s{_ttSourceLanguageCode = a})

-- | The language code requested for the language of the target text. The language must be a language supported by Amazon Translate.
ttTargetLanguageCode :: Lens' TranslateText Text
ttTargetLanguageCode = lens _ttTargetLanguageCode (\ s a -> s{_ttTargetLanguageCode = a})

instance AWSRequest TranslateText where
        type Rs TranslateText = TranslateTextResponse
        request = postJSON translate
        response
          = receiveJSON
              (\ s h x ->
                 TranslateTextResponse' <$>
                   (x .?> "AppliedTerminologies" .!@ mempty) <*>
                     (pure (fromEnum s))
                     <*> (x .:> "TranslatedText")
                     <*> (x .:> "SourceLanguageCode")
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
                 [("TerminologyNames" .=) <$> _ttTerminologyNames,
                  Just ("Text" .= _ttText),
                  Just ("SourceLanguageCode" .= _ttSourceLanguageCode),
                  Just
                    ("TargetLanguageCode" .= _ttTargetLanguageCode)])

instance ToPath TranslateText where
        toPath = const "/"

instance ToQuery TranslateText where
        toQuery = const mempty

-- | /See:/ 'translateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { _ttrsAppliedTerminologies :: !(Maybe [AppliedTerminology])
  , _ttrsResponseStatus       :: !Int
  , _ttrsTranslatedText       :: !Text
  , _ttrsSourceLanguageCode   :: !Text
  , _ttrsTargetLanguageCode   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TranslateTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrsAppliedTerminologies' - The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
--
-- * 'ttrsResponseStatus' - -- | The response status code.
--
-- * 'ttrsTranslatedText' - The the translated text. The maximum length of this text is 5kb.
--
-- * 'ttrsSourceLanguageCode' - The language code for the language of the source text.
--
-- * 'ttrsTargetLanguageCode' - The language code for the language of the target text.
translateTextResponse
    :: Int -- ^ 'ttrsResponseStatus'
    -> Text -- ^ 'ttrsTranslatedText'
    -> Text -- ^ 'ttrsSourceLanguageCode'
    -> Text -- ^ 'ttrsTargetLanguageCode'
    -> TranslateTextResponse
translateTextResponse pResponseStatus_ pTranslatedText_ pSourceLanguageCode_ pTargetLanguageCode_ =
  TranslateTextResponse'
    { _ttrsAppliedTerminologies = Nothing
    , _ttrsResponseStatus = pResponseStatus_
    , _ttrsTranslatedText = pTranslatedText_
    , _ttrsSourceLanguageCode = pSourceLanguageCode_
    , _ttrsTargetLanguageCode = pTargetLanguageCode_
    }


-- | The names of the custom terminologies applied to the input text by Amazon Translate for the translated text response.
ttrsAppliedTerminologies :: Lens' TranslateTextResponse [AppliedTerminology]
ttrsAppliedTerminologies = lens _ttrsAppliedTerminologies (\ s a -> s{_ttrsAppliedTerminologies = a}) . _Default . _Coerce

-- | -- | The response status code.
ttrsResponseStatus :: Lens' TranslateTextResponse Int
ttrsResponseStatus = lens _ttrsResponseStatus (\ s a -> s{_ttrsResponseStatus = a})

-- | The the translated text. The maximum length of this text is 5kb.
ttrsTranslatedText :: Lens' TranslateTextResponse Text
ttrsTranslatedText = lens _ttrsTranslatedText (\ s a -> s{_ttrsTranslatedText = a})

-- | The language code for the language of the source text.
ttrsSourceLanguageCode :: Lens' TranslateTextResponse Text
ttrsSourceLanguageCode = lens _ttrsSourceLanguageCode (\ s a -> s{_ttrsSourceLanguageCode = a})

-- | The language code for the language of the target text.
ttrsTargetLanguageCode :: Lens' TranslateTextResponse Text
ttrsTargetLanguageCode = lens _ttrsTargetLanguageCode (\ s a -> s{_ttrsTargetLanguageCode = a})

instance NFData TranslateTextResponse where
