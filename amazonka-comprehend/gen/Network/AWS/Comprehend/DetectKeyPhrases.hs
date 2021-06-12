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
-- Module      : Network.AWS.Comprehend.DetectKeyPhrases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in the text.
module Network.AWS.Comprehend.DetectKeyPhrases
  ( -- * Creating a Request
    DetectKeyPhrases (..),
    newDetectKeyPhrases,

    -- * Request Lenses
    detectKeyPhrases_text,
    detectKeyPhrases_languageCode,

    -- * Destructuring the Response
    DetectKeyPhrasesResponse (..),
    newDetectKeyPhrasesResponse,

    -- * Response Lenses
    detectKeyPhrasesResponse_keyPhrases,
    detectKeyPhrasesResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
    -- UTF-8 encoded characters.
    text :: Core.Sensitive Core.Text,
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectKeyPhrases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectKeyPhrases_text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
--
-- 'languageCode', 'detectKeyPhrases_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newDetectKeyPhrases ::
  -- | 'text'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectKeyPhrases
newDetectKeyPhrases pText_ pLanguageCode_ =
  DetectKeyPhrases'
    { text =
        Core._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
detectKeyPhrases_text :: Lens.Lens' DetectKeyPhrases Core.Text
detectKeyPhrases_text = Lens.lens (\DetectKeyPhrases' {text} -> text) (\s@DetectKeyPhrases' {} a -> s {text = a} :: DetectKeyPhrases) Core.. Core._Sensitive

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
detectKeyPhrases_languageCode :: Lens.Lens' DetectKeyPhrases LanguageCode
detectKeyPhrases_languageCode = Lens.lens (\DetectKeyPhrases' {languageCode} -> languageCode) (\s@DetectKeyPhrases' {} a -> s {languageCode = a} :: DetectKeyPhrases)

instance Core.AWSRequest DetectKeyPhrases where
  type
    AWSResponse DetectKeyPhrases =
      DetectKeyPhrasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectKeyPhrasesResponse'
            Core.<$> (x Core..?> "KeyPhrases" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetectKeyPhrases

instance Core.NFData DetectKeyPhrases

instance Core.ToHeaders DetectKeyPhrases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectKeyPhrases" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetectKeyPhrases where
  toJSON DetectKeyPhrases' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectKeyPhrases where
  toPath = Core.const "/"

instance Core.ToQuery DetectKeyPhrases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetectKeyPhrasesResponse' smart constructor.
data DetectKeyPhrasesResponse = DetectKeyPhrasesResponse'
  { -- | A collection of key phrases that Amazon Comprehend identified in the
    -- input text. For each key phrase, the response provides the text of the
    -- key phrase, where the key phrase begins and ends, and the level of
    -- confidence that Amazon Comprehend has in the accuracy of the detection.
    keyPhrases :: Core.Maybe [KeyPhrase],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectKeyPhrasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPhrases', 'detectKeyPhrasesResponse_keyPhrases' - A collection of key phrases that Amazon Comprehend identified in the
-- input text. For each key phrase, the response provides the text of the
-- key phrase, where the key phrase begins and ends, and the level of
-- confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- 'httpStatus', 'detectKeyPhrasesResponse_httpStatus' - The response's http status code.
newDetectKeyPhrasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetectKeyPhrasesResponse
newDetectKeyPhrasesResponse pHttpStatus_ =
  DetectKeyPhrasesResponse'
    { keyPhrases =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key phrases that Amazon Comprehend identified in the
-- input text. For each key phrase, the response provides the text of the
-- key phrase, where the key phrase begins and ends, and the level of
-- confidence that Amazon Comprehend has in the accuracy of the detection.
detectKeyPhrasesResponse_keyPhrases :: Lens.Lens' DetectKeyPhrasesResponse (Core.Maybe [KeyPhrase])
detectKeyPhrasesResponse_keyPhrases = Lens.lens (\DetectKeyPhrasesResponse' {keyPhrases} -> keyPhrases) (\s@DetectKeyPhrasesResponse' {} a -> s {keyPhrases = a} :: DetectKeyPhrasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectKeyPhrasesResponse_httpStatus :: Lens.Lens' DetectKeyPhrasesResponse Core.Int
detectKeyPhrasesResponse_httpStatus = Lens.lens (\DetectKeyPhrasesResponse' {httpStatus} -> httpStatus) (\s@DetectKeyPhrasesResponse' {} a -> s {httpStatus = a} :: DetectKeyPhrasesResponse)

instance Core.NFData DetectKeyPhrasesResponse
