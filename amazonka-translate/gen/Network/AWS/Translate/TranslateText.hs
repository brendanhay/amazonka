{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.TranslateText
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates input text from the source language to the target language.
-- For a list of available languages and language codes, see
-- what-is-languages.
module Network.AWS.Translate.TranslateText
  ( -- * Creating a Request
    TranslateText (..),
    newTranslateText,

    -- * Request Lenses
    translateText_terminologyNames,
    translateText_text,
    translateText_sourceLanguageCode,
    translateText_targetLanguageCode,

    -- * Destructuring the Response
    TranslateTextResponse (..),
    newTranslateTextResponse,

    -- * Response Lenses
    translateTextResponse_appliedTerminologies,
    translateTextResponse_httpStatus,
    translateTextResponse_translatedText,
    translateTextResponse_sourceLanguageCode,
    translateTextResponse_targetLanguageCode,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newTranslateText' smart constructor.
data TranslateText = TranslateText'
  { -- | The name of the terminology list file to be used in the TranslateText
    -- request. You can use 1 terminology list at most in a @TranslateText@
    -- request. Terminology lists can contain a maximum of 256 terms.
    terminologyNames :: Core.Maybe [Core.Text],
    -- | The text to translate. The text string can be a maximum of 5,000 bytes
    -- long. Depending on your character set, this may be fewer than 5,000
    -- characters.
    text :: Core.Text,
    -- | The language code for the language of the source text. The language must
    -- be a language supported by Amazon Translate. For a list of language
    -- codes, see what-is-languages.
    --
    -- To have Amazon Translate determine the source language of your text, you
    -- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
    -- @auto@, Amazon Translate will call
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
    -- to determine the source language.
    sourceLanguageCode :: Core.Text,
    -- | The language code requested for the language of the target text. The
    -- language must be a language supported by Amazon Translate.
    targetLanguageCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TranslateText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminologyNames', 'translateText_terminologyNames' - The name of the terminology list file to be used in the TranslateText
-- request. You can use 1 terminology list at most in a @TranslateText@
-- request. Terminology lists can contain a maximum of 256 terms.
--
-- 'text', 'translateText_text' - The text to translate. The text string can be a maximum of 5,000 bytes
-- long. Depending on your character set, this may be fewer than 5,000
-- characters.
--
-- 'sourceLanguageCode', 'translateText_sourceLanguageCode' - The language code for the language of the source text. The language must
-- be a language supported by Amazon Translate. For a list of language
-- codes, see what-is-languages.
--
-- To have Amazon Translate determine the source language of your text, you
-- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
-- @auto@, Amazon Translate will call
-- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
-- to determine the source language.
--
-- 'targetLanguageCode', 'translateText_targetLanguageCode' - The language code requested for the language of the target text. The
-- language must be a language supported by Amazon Translate.
newTranslateText ::
  -- | 'text'
  Core.Text ->
  -- | 'sourceLanguageCode'
  Core.Text ->
  -- | 'targetLanguageCode'
  Core.Text ->
  TranslateText
newTranslateText
  pText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateText'
      { terminologyNames = Core.Nothing,
        text = pText_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | The name of the terminology list file to be used in the TranslateText
-- request. You can use 1 terminology list at most in a @TranslateText@
-- request. Terminology lists can contain a maximum of 256 terms.
translateText_terminologyNames :: Lens.Lens' TranslateText (Core.Maybe [Core.Text])
translateText_terminologyNames = Lens.lens (\TranslateText' {terminologyNames} -> terminologyNames) (\s@TranslateText' {} a -> s {terminologyNames = a} :: TranslateText) Core.. Lens.mapping Lens._Coerce

-- | The text to translate. The text string can be a maximum of 5,000 bytes
-- long. Depending on your character set, this may be fewer than 5,000
-- characters.
translateText_text :: Lens.Lens' TranslateText Core.Text
translateText_text = Lens.lens (\TranslateText' {text} -> text) (\s@TranslateText' {} a -> s {text = a} :: TranslateText)

-- | The language code for the language of the source text. The language must
-- be a language supported by Amazon Translate. For a list of language
-- codes, see what-is-languages.
--
-- To have Amazon Translate determine the source language of your text, you
-- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
-- @auto@, Amazon Translate will call
-- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
-- to determine the source language.
translateText_sourceLanguageCode :: Lens.Lens' TranslateText Core.Text
translateText_sourceLanguageCode = Lens.lens (\TranslateText' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateText' {} a -> s {sourceLanguageCode = a} :: TranslateText)

-- | The language code requested for the language of the target text. The
-- language must be a language supported by Amazon Translate.
translateText_targetLanguageCode :: Lens.Lens' TranslateText Core.Text
translateText_targetLanguageCode = Lens.lens (\TranslateText' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateText' {} a -> s {targetLanguageCode = a} :: TranslateText)

instance Core.AWSRequest TranslateText where
  type
    AWSResponse TranslateText =
      TranslateTextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TranslateTextResponse'
            Core.<$> ( x Core..?> "AppliedTerminologies"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "TranslatedText")
            Core.<*> (x Core..:> "SourceLanguageCode")
            Core.<*> (x Core..:> "TargetLanguageCode")
      )

instance Core.Hashable TranslateText

instance Core.NFData TranslateText

instance Core.ToHeaders TranslateText where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.TranslateText" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TranslateText where
  toJSON TranslateText' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TerminologyNames" Core..=)
              Core.<$> terminologyNames,
            Core.Just ("Text" Core..= text),
            Core.Just
              ("SourceLanguageCode" Core..= sourceLanguageCode),
            Core.Just
              ("TargetLanguageCode" Core..= targetLanguageCode)
          ]
      )

instance Core.ToPath TranslateText where
  toPath = Core.const "/"

instance Core.ToQuery TranslateText where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTranslateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { -- | The names of the custom terminologies applied to the input text by
    -- Amazon Translate for the translated text response.
    appliedTerminologies :: Core.Maybe [AppliedTerminology],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The translated text.
    translatedText :: Core.Text,
    -- | The language code for the language of the source text.
    sourceLanguageCode :: Core.Text,
    -- | The language code for the language of the target text.
    targetLanguageCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TranslateTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedTerminologies', 'translateTextResponse_appliedTerminologies' - The names of the custom terminologies applied to the input text by
-- Amazon Translate for the translated text response.
--
-- 'httpStatus', 'translateTextResponse_httpStatus' - The response's http status code.
--
-- 'translatedText', 'translateTextResponse_translatedText' - The translated text.
--
-- 'sourceLanguageCode', 'translateTextResponse_sourceLanguageCode' - The language code for the language of the source text.
--
-- 'targetLanguageCode', 'translateTextResponse_targetLanguageCode' - The language code for the language of the target text.
newTranslateTextResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'translatedText'
  Core.Text ->
  -- | 'sourceLanguageCode'
  Core.Text ->
  -- | 'targetLanguageCode'
  Core.Text ->
  TranslateTextResponse
newTranslateTextResponse
  pHttpStatus_
  pTranslatedText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateTextResponse'
      { appliedTerminologies =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        translatedText = pTranslatedText_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | The names of the custom terminologies applied to the input text by
-- Amazon Translate for the translated text response.
translateTextResponse_appliedTerminologies :: Lens.Lens' TranslateTextResponse (Core.Maybe [AppliedTerminology])
translateTextResponse_appliedTerminologies = Lens.lens (\TranslateTextResponse' {appliedTerminologies} -> appliedTerminologies) (\s@TranslateTextResponse' {} a -> s {appliedTerminologies = a} :: TranslateTextResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
translateTextResponse_httpStatus :: Lens.Lens' TranslateTextResponse Core.Int
translateTextResponse_httpStatus = Lens.lens (\TranslateTextResponse' {httpStatus} -> httpStatus) (\s@TranslateTextResponse' {} a -> s {httpStatus = a} :: TranslateTextResponse)

-- | The translated text.
translateTextResponse_translatedText :: Lens.Lens' TranslateTextResponse Core.Text
translateTextResponse_translatedText = Lens.lens (\TranslateTextResponse' {translatedText} -> translatedText) (\s@TranslateTextResponse' {} a -> s {translatedText = a} :: TranslateTextResponse)

-- | The language code for the language of the source text.
translateTextResponse_sourceLanguageCode :: Lens.Lens' TranslateTextResponse Core.Text
translateTextResponse_sourceLanguageCode = Lens.lens (\TranslateTextResponse' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateTextResponse' {} a -> s {sourceLanguageCode = a} :: TranslateTextResponse)

-- | The language code for the language of the target text.
translateTextResponse_targetLanguageCode :: Lens.Lens' TranslateTextResponse Core.Text
translateTextResponse_targetLanguageCode = Lens.lens (\TranslateTextResponse' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateTextResponse' {} a -> s {targetLanguageCode = a} :: TranslateTextResponse)

instance Core.NFData TranslateTextResponse
