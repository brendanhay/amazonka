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
-- Module      : Amazonka.Translate.TranslateText
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates input text from the source language to the target language.
-- For a list of available languages and language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
module Amazonka.Translate.TranslateText
  ( -- * Creating a Request
    TranslateText (..),
    newTranslateText,

    -- * Request Lenses
    translateText_settings,
    translateText_terminologyNames,
    translateText_text,
    translateText_sourceLanguageCode,
    translateText_targetLanguageCode,

    -- * Destructuring the Response
    TranslateTextResponse (..),
    newTranslateTextResponse,

    -- * Response Lenses
    translateTextResponse_appliedSettings,
    translateTextResponse_appliedTerminologies,
    translateTextResponse_httpStatus,
    translateTextResponse_translatedText,
    translateTextResponse_sourceLanguageCode,
    translateTextResponse_targetLanguageCode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newTranslateText' smart constructor.
data TranslateText = TranslateText'
  { -- | Settings to configure your translation output, including the option to
    -- set the formality level of the output text and the option to mask
    -- profane words and phrases.
    settings :: Prelude.Maybe TranslationSettings,
    -- | The name of the terminology list file to be used in the TranslateText
    -- request. You can use 1 terminology list at most in a @TranslateText@
    -- request. Terminology lists can contain a maximum of 256 terms.
    terminologyNames :: Prelude.Maybe [Prelude.Text],
    -- | The text to translate. The text string can be a maximum of 5,000 bytes
    -- long. Depending on your character set, this may be fewer than 5,000
    -- characters.
    text :: Prelude.Text,
    -- | The language code for the language of the source text. The language must
    -- be a language supported by Amazon Translate. For a list of language
    -- codes, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
    --
    -- To have Amazon Translate determine the source language of your text, you
    -- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
    -- @auto@, Amazon Translate will call
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
    -- to determine the source language.
    --
    -- If you specify @auto@, you must send the @TranslateText@ request in a
    -- region that supports Amazon Comprehend. Otherwise, the request returns
    -- an error indicating that autodetect is not supported.
    sourceLanguageCode :: Prelude.Text,
    -- | The language code requested for the language of the target text. The
    -- language must be a language supported by Amazon Translate.
    targetLanguageCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslateText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'translateText_settings' - Settings to configure your translation output, including the option to
-- set the formality level of the output text and the option to mask
-- profane words and phrases.
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
-- codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- To have Amazon Translate determine the source language of your text, you
-- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
-- @auto@, Amazon Translate will call
-- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
-- to determine the source language.
--
-- If you specify @auto@, you must send the @TranslateText@ request in a
-- region that supports Amazon Comprehend. Otherwise, the request returns
-- an error indicating that autodetect is not supported.
--
-- 'targetLanguageCode', 'translateText_targetLanguageCode' - The language code requested for the language of the target text. The
-- language must be a language supported by Amazon Translate.
newTranslateText ::
  -- | 'text'
  Prelude.Text ->
  -- | 'sourceLanguageCode'
  Prelude.Text ->
  -- | 'targetLanguageCode'
  Prelude.Text ->
  TranslateText
newTranslateText
  pText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateText'
      { settings = Prelude.Nothing,
        terminologyNames = Prelude.Nothing,
        text = pText_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | Settings to configure your translation output, including the option to
-- set the formality level of the output text and the option to mask
-- profane words and phrases.
translateText_settings :: Lens.Lens' TranslateText (Prelude.Maybe TranslationSettings)
translateText_settings = Lens.lens (\TranslateText' {settings} -> settings) (\s@TranslateText' {} a -> s {settings = a} :: TranslateText)

-- | The name of the terminology list file to be used in the TranslateText
-- request. You can use 1 terminology list at most in a @TranslateText@
-- request. Terminology lists can contain a maximum of 256 terms.
translateText_terminologyNames :: Lens.Lens' TranslateText (Prelude.Maybe [Prelude.Text])
translateText_terminologyNames = Lens.lens (\TranslateText' {terminologyNames} -> terminologyNames) (\s@TranslateText' {} a -> s {terminologyNames = a} :: TranslateText) Prelude.. Lens.mapping Lens.coerced

-- | The text to translate. The text string can be a maximum of 5,000 bytes
-- long. Depending on your character set, this may be fewer than 5,000
-- characters.
translateText_text :: Lens.Lens' TranslateText Prelude.Text
translateText_text = Lens.lens (\TranslateText' {text} -> text) (\s@TranslateText' {} a -> s {text = a} :: TranslateText)

-- | The language code for the language of the source text. The language must
-- be a language supported by Amazon Translate. For a list of language
-- codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- To have Amazon Translate determine the source language of your text, you
-- can specify @auto@ in the @SourceLanguageCode@ field. If you specify
-- @auto@, Amazon Translate will call
-- <https://docs.aws.amazon.com/comprehend/latest/dg/comprehend-general.html Amazon Comprehend>
-- to determine the source language.
--
-- If you specify @auto@, you must send the @TranslateText@ request in a
-- region that supports Amazon Comprehend. Otherwise, the request returns
-- an error indicating that autodetect is not supported.
translateText_sourceLanguageCode :: Lens.Lens' TranslateText Prelude.Text
translateText_sourceLanguageCode = Lens.lens (\TranslateText' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateText' {} a -> s {sourceLanguageCode = a} :: TranslateText)

-- | The language code requested for the language of the target text. The
-- language must be a language supported by Amazon Translate.
translateText_targetLanguageCode :: Lens.Lens' TranslateText Prelude.Text
translateText_targetLanguageCode = Lens.lens (\TranslateText' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateText' {} a -> s {targetLanguageCode = a} :: TranslateText)

instance Core.AWSRequest TranslateText where
  type
    AWSResponse TranslateText =
      TranslateTextResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TranslateTextResponse'
            Prelude.<$> (x Data..?> "AppliedSettings")
            Prelude.<*> ( x Data..?> "AppliedTerminologies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TranslatedText")
            Prelude.<*> (x Data..:> "SourceLanguageCode")
            Prelude.<*> (x Data..:> "TargetLanguageCode")
      )

instance Prelude.Hashable TranslateText where
  hashWithSalt _salt TranslateText' {..} =
    _salt `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` terminologyNames
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` targetLanguageCode

instance Prelude.NFData TranslateText where
  rnf TranslateText' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf terminologyNames
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf targetLanguageCode

instance Data.ToHeaders TranslateText where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.TranslateText" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TranslateText where
  toJSON TranslateText' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Settings" Data..=) Prelude.<$> settings,
            ("TerminologyNames" Data..=)
              Prelude.<$> terminologyNames,
            Prelude.Just ("Text" Data..= text),
            Prelude.Just
              ("SourceLanguageCode" Data..= sourceLanguageCode),
            Prelude.Just
              ("TargetLanguageCode" Data..= targetLanguageCode)
          ]
      )

instance Data.ToPath TranslateText where
  toPath = Prelude.const "/"

instance Data.ToQuery TranslateText where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTranslateTextResponse' smart constructor.
data TranslateTextResponse = TranslateTextResponse'
  { -- | Settings that configure the translation output.
    appliedSettings :: Prelude.Maybe TranslationSettings,
    -- | The names of the custom terminologies applied to the input text by
    -- Amazon Translate for the translated text response.
    appliedTerminologies :: Prelude.Maybe [AppliedTerminology],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The translated text.
    translatedText :: Prelude.Text,
    -- | The language code for the language of the source text.
    sourceLanguageCode :: Prelude.Text,
    -- | The language code for the language of the target text.
    targetLanguageCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslateTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedSettings', 'translateTextResponse_appliedSettings' - Settings that configure the translation output.
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
  Prelude.Int ->
  -- | 'translatedText'
  Prelude.Text ->
  -- | 'sourceLanguageCode'
  Prelude.Text ->
  -- | 'targetLanguageCode'
  Prelude.Text ->
  TranslateTextResponse
newTranslateTextResponse
  pHttpStatus_
  pTranslatedText_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateTextResponse'
      { appliedSettings =
          Prelude.Nothing,
        appliedTerminologies = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        translatedText = pTranslatedText_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | Settings that configure the translation output.
translateTextResponse_appliedSettings :: Lens.Lens' TranslateTextResponse (Prelude.Maybe TranslationSettings)
translateTextResponse_appliedSettings = Lens.lens (\TranslateTextResponse' {appliedSettings} -> appliedSettings) (\s@TranslateTextResponse' {} a -> s {appliedSettings = a} :: TranslateTextResponse)

-- | The names of the custom terminologies applied to the input text by
-- Amazon Translate for the translated text response.
translateTextResponse_appliedTerminologies :: Lens.Lens' TranslateTextResponse (Prelude.Maybe [AppliedTerminology])
translateTextResponse_appliedTerminologies = Lens.lens (\TranslateTextResponse' {appliedTerminologies} -> appliedTerminologies) (\s@TranslateTextResponse' {} a -> s {appliedTerminologies = a} :: TranslateTextResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
translateTextResponse_httpStatus :: Lens.Lens' TranslateTextResponse Prelude.Int
translateTextResponse_httpStatus = Lens.lens (\TranslateTextResponse' {httpStatus} -> httpStatus) (\s@TranslateTextResponse' {} a -> s {httpStatus = a} :: TranslateTextResponse)

-- | The translated text.
translateTextResponse_translatedText :: Lens.Lens' TranslateTextResponse Prelude.Text
translateTextResponse_translatedText = Lens.lens (\TranslateTextResponse' {translatedText} -> translatedText) (\s@TranslateTextResponse' {} a -> s {translatedText = a} :: TranslateTextResponse)

-- | The language code for the language of the source text.
translateTextResponse_sourceLanguageCode :: Lens.Lens' TranslateTextResponse Prelude.Text
translateTextResponse_sourceLanguageCode = Lens.lens (\TranslateTextResponse' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateTextResponse' {} a -> s {sourceLanguageCode = a} :: TranslateTextResponse)

-- | The language code for the language of the target text.
translateTextResponse_targetLanguageCode :: Lens.Lens' TranslateTextResponse Prelude.Text
translateTextResponse_targetLanguageCode = Lens.lens (\TranslateTextResponse' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateTextResponse' {} a -> s {targetLanguageCode = a} :: TranslateTextResponse)

instance Prelude.NFData TranslateTextResponse where
  rnf TranslateTextResponse' {..} =
    Prelude.rnf appliedSettings
      `Prelude.seq` Prelude.rnf appliedTerminologies
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf translatedText
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf targetLanguageCode
