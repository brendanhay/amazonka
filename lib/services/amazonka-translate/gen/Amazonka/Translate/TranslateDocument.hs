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
-- Module      : Amazonka.Translate.TranslateDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Translates the input document from the source language to the target
-- language. This synchronous operation supports plain text or HTML for the
-- input document. @TranslateDocument@ supports translations from English
-- to any supported language, and from any supported language to English.
-- Therefore, specify either the source language code or the target
-- language code as “en” (English).
--
-- @TranslateDocument@ does not support language auto-detection.
--
-- If you set the @Formality@ parameter, the request will fail if the
-- target language does not support formality. For a list of target
-- languages that support formality, see
-- <https://docs.aws.amazon.com/translate/latest/dg/customizing-translations-formality.html Setting formality>.
module Amazonka.Translate.TranslateDocument
  ( -- * Creating a Request
    TranslateDocument (..),
    newTranslateDocument,

    -- * Request Lenses
    translateDocument_settings,
    translateDocument_terminologyNames,
    translateDocument_document,
    translateDocument_sourceLanguageCode,
    translateDocument_targetLanguageCode,

    -- * Destructuring the Response
    TranslateDocumentResponse (..),
    newTranslateDocumentResponse,

    -- * Response Lenses
    translateDocumentResponse_appliedSettings,
    translateDocumentResponse_appliedTerminologies,
    translateDocumentResponse_httpStatus,
    translateDocumentResponse_translatedDocument,
    translateDocumentResponse_sourceLanguageCode,
    translateDocumentResponse_targetLanguageCode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newTranslateDocument' smart constructor.
data TranslateDocument = TranslateDocument'
  { settings :: Prelude.Maybe TranslationSettings,
    -- | The name of a terminology list file to add to the translation job. This
    -- file provides source terms and the desired translation for each term. A
    -- terminology list can contain a maximum of 256 terms. You can use one
    -- custom terminology resource in your translation request.
    --
    -- Use the ListTerminologies operation to get the available terminology
    -- lists.
    --
    -- For more information about custom terminology lists, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
    terminologyNames :: Prelude.Maybe [Prelude.Text],
    -- | The content and content type for the document to be translated. The
    -- document size must not exceed 100 KB.
    document :: Document,
    -- | The language code for the language of the source text. Do not use
    -- @auto@, because @TranslateDocument@ does not support language
    -- auto-detection. For a list of supported language codes, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
    sourceLanguageCode :: Prelude.Text,
    -- | The language code requested for the translated document. For a list of
    -- supported language codes, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
    targetLanguageCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'translateDocument_settings' - Undocumented member.
--
-- 'terminologyNames', 'translateDocument_terminologyNames' - The name of a terminology list file to add to the translation job. This
-- file provides source terms and the desired translation for each term. A
-- terminology list can contain a maximum of 256 terms. You can use one
-- custom terminology resource in your translation request.
--
-- Use the ListTerminologies operation to get the available terminology
-- lists.
--
-- For more information about custom terminology lists, see
-- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
--
-- 'document', 'translateDocument_document' - The content and content type for the document to be translated. The
-- document size must not exceed 100 KB.
--
-- 'sourceLanguageCode', 'translateDocument_sourceLanguageCode' - The language code for the language of the source text. Do not use
-- @auto@, because @TranslateDocument@ does not support language
-- auto-detection. For a list of supported language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- 'targetLanguageCode', 'translateDocument_targetLanguageCode' - The language code requested for the translated document. For a list of
-- supported language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
newTranslateDocument ::
  -- | 'document'
  Document ->
  -- | 'sourceLanguageCode'
  Prelude.Text ->
  -- | 'targetLanguageCode'
  Prelude.Text ->
  TranslateDocument
newTranslateDocument
  pDocument_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateDocument'
      { settings = Prelude.Nothing,
        terminologyNames = Prelude.Nothing,
        document = pDocument_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | Undocumented member.
translateDocument_settings :: Lens.Lens' TranslateDocument (Prelude.Maybe TranslationSettings)
translateDocument_settings = Lens.lens (\TranslateDocument' {settings} -> settings) (\s@TranslateDocument' {} a -> s {settings = a} :: TranslateDocument)

-- | The name of a terminology list file to add to the translation job. This
-- file provides source terms and the desired translation for each term. A
-- terminology list can contain a maximum of 256 terms. You can use one
-- custom terminology resource in your translation request.
--
-- Use the ListTerminologies operation to get the available terminology
-- lists.
--
-- For more information about custom terminology lists, see
-- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
translateDocument_terminologyNames :: Lens.Lens' TranslateDocument (Prelude.Maybe [Prelude.Text])
translateDocument_terminologyNames = Lens.lens (\TranslateDocument' {terminologyNames} -> terminologyNames) (\s@TranslateDocument' {} a -> s {terminologyNames = a} :: TranslateDocument) Prelude.. Lens.mapping Lens.coerced

-- | The content and content type for the document to be translated. The
-- document size must not exceed 100 KB.
translateDocument_document :: Lens.Lens' TranslateDocument Document
translateDocument_document = Lens.lens (\TranslateDocument' {document} -> document) (\s@TranslateDocument' {} a -> s {document = a} :: TranslateDocument)

-- | The language code for the language of the source text. Do not use
-- @auto@, because @TranslateDocument@ does not support language
-- auto-detection. For a list of supported language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
translateDocument_sourceLanguageCode :: Lens.Lens' TranslateDocument Prelude.Text
translateDocument_sourceLanguageCode = Lens.lens (\TranslateDocument' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateDocument' {} a -> s {sourceLanguageCode = a} :: TranslateDocument)

-- | The language code requested for the translated document. For a list of
-- supported language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
translateDocument_targetLanguageCode :: Lens.Lens' TranslateDocument Prelude.Text
translateDocument_targetLanguageCode = Lens.lens (\TranslateDocument' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateDocument' {} a -> s {targetLanguageCode = a} :: TranslateDocument)

instance Core.AWSRequest TranslateDocument where
  type
    AWSResponse TranslateDocument =
      TranslateDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TranslateDocumentResponse'
            Prelude.<$> (x Data..?> "AppliedSettings")
            Prelude.<*> ( x
                            Data..?> "AppliedTerminologies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TranslatedDocument")
            Prelude.<*> (x Data..:> "SourceLanguageCode")
            Prelude.<*> (x Data..:> "TargetLanguageCode")
      )

instance Prelude.Hashable TranslateDocument where
  hashWithSalt _salt TranslateDocument' {..} =
    _salt
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` terminologyNames
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` targetLanguageCode

instance Prelude.NFData TranslateDocument where
  rnf TranslateDocument' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf terminologyNames
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf targetLanguageCode

instance Data.ToHeaders TranslateDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.TranslateDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TranslateDocument where
  toJSON TranslateDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Settings" Data..=) Prelude.<$> settings,
            ("TerminologyNames" Data..=)
              Prelude.<$> terminologyNames,
            Prelude.Just ("Document" Data..= document),
            Prelude.Just
              ("SourceLanguageCode" Data..= sourceLanguageCode),
            Prelude.Just
              ("TargetLanguageCode" Data..= targetLanguageCode)
          ]
      )

instance Data.ToPath TranslateDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery TranslateDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTranslateDocumentResponse' smart constructor.
data TranslateDocumentResponse = TranslateDocumentResponse'
  { appliedSettings :: Prelude.Maybe TranslationSettings,
    -- | The names of the custom terminologies applied to the input text by
    -- Amazon Translate to produce the translated text document.
    appliedTerminologies :: Prelude.Maybe [AppliedTerminology],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The document containing the translated content. The document format
    -- matches the source document format.
    translatedDocument :: TranslatedDocument,
    -- | The language code of the source document.
    sourceLanguageCode :: Prelude.Text,
    -- | The language code of the translated document.
    targetLanguageCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedSettings', 'translateDocumentResponse_appliedSettings' - Undocumented member.
--
-- 'appliedTerminologies', 'translateDocumentResponse_appliedTerminologies' - The names of the custom terminologies applied to the input text by
-- Amazon Translate to produce the translated text document.
--
-- 'httpStatus', 'translateDocumentResponse_httpStatus' - The response's http status code.
--
-- 'translatedDocument', 'translateDocumentResponse_translatedDocument' - The document containing the translated content. The document format
-- matches the source document format.
--
-- 'sourceLanguageCode', 'translateDocumentResponse_sourceLanguageCode' - The language code of the source document.
--
-- 'targetLanguageCode', 'translateDocumentResponse_targetLanguageCode' - The language code of the translated document.
newTranslateDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'translatedDocument'
  TranslatedDocument ->
  -- | 'sourceLanguageCode'
  Prelude.Text ->
  -- | 'targetLanguageCode'
  Prelude.Text ->
  TranslateDocumentResponse
newTranslateDocumentResponse
  pHttpStatus_
  pTranslatedDocument_
  pSourceLanguageCode_
  pTargetLanguageCode_ =
    TranslateDocumentResponse'
      { appliedSettings =
          Prelude.Nothing,
        appliedTerminologies = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        translatedDocument = pTranslatedDocument_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCode = pTargetLanguageCode_
      }

-- | Undocumented member.
translateDocumentResponse_appliedSettings :: Lens.Lens' TranslateDocumentResponse (Prelude.Maybe TranslationSettings)
translateDocumentResponse_appliedSettings = Lens.lens (\TranslateDocumentResponse' {appliedSettings} -> appliedSettings) (\s@TranslateDocumentResponse' {} a -> s {appliedSettings = a} :: TranslateDocumentResponse)

-- | The names of the custom terminologies applied to the input text by
-- Amazon Translate to produce the translated text document.
translateDocumentResponse_appliedTerminologies :: Lens.Lens' TranslateDocumentResponse (Prelude.Maybe [AppliedTerminology])
translateDocumentResponse_appliedTerminologies = Lens.lens (\TranslateDocumentResponse' {appliedTerminologies} -> appliedTerminologies) (\s@TranslateDocumentResponse' {} a -> s {appliedTerminologies = a} :: TranslateDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
translateDocumentResponse_httpStatus :: Lens.Lens' TranslateDocumentResponse Prelude.Int
translateDocumentResponse_httpStatus = Lens.lens (\TranslateDocumentResponse' {httpStatus} -> httpStatus) (\s@TranslateDocumentResponse' {} a -> s {httpStatus = a} :: TranslateDocumentResponse)

-- | The document containing the translated content. The document format
-- matches the source document format.
translateDocumentResponse_translatedDocument :: Lens.Lens' TranslateDocumentResponse TranslatedDocument
translateDocumentResponse_translatedDocument = Lens.lens (\TranslateDocumentResponse' {translatedDocument} -> translatedDocument) (\s@TranslateDocumentResponse' {} a -> s {translatedDocument = a} :: TranslateDocumentResponse)

-- | The language code of the source document.
translateDocumentResponse_sourceLanguageCode :: Lens.Lens' TranslateDocumentResponse Prelude.Text
translateDocumentResponse_sourceLanguageCode = Lens.lens (\TranslateDocumentResponse' {sourceLanguageCode} -> sourceLanguageCode) (\s@TranslateDocumentResponse' {} a -> s {sourceLanguageCode = a} :: TranslateDocumentResponse)

-- | The language code of the translated document.
translateDocumentResponse_targetLanguageCode :: Lens.Lens' TranslateDocumentResponse Prelude.Text
translateDocumentResponse_targetLanguageCode = Lens.lens (\TranslateDocumentResponse' {targetLanguageCode} -> targetLanguageCode) (\s@TranslateDocumentResponse' {} a -> s {targetLanguageCode = a} :: TranslateDocumentResponse)

instance Prelude.NFData TranslateDocumentResponse where
  rnf TranslateDocumentResponse' {..} =
    Prelude.rnf appliedSettings
      `Prelude.seq` Prelude.rnf appliedTerminologies
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf translatedDocument
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf targetLanguageCode
