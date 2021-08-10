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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
    -- UTF-8 encoded characters.
    text :: Core.Sensitive Prelude.Text,
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
detectKeyPhrases_text :: Lens.Lens' DetectKeyPhrases Prelude.Text
detectKeyPhrases_text = Lens.lens (\DetectKeyPhrases' {text} -> text) (\s@DetectKeyPhrases' {} a -> s {text = a} :: DetectKeyPhrases) Prelude.. Core._Sensitive

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
            Prelude.<$> (x Core..?> "KeyPhrases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectKeyPhrases

instance Prelude.NFData DetectKeyPhrases

instance Core.ToHeaders DetectKeyPhrases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectKeyPhrases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectKeyPhrases where
  toJSON DetectKeyPhrases' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Core..= text),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectKeyPhrases where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectKeyPhrases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectKeyPhrasesResponse' smart constructor.
data DetectKeyPhrasesResponse = DetectKeyPhrasesResponse'
  { -- | A collection of key phrases that Amazon Comprehend identified in the
    -- input text. For each key phrase, the response provides the text of the
    -- key phrase, where the key phrase begins and ends, and the level of
    -- confidence that Amazon Comprehend has in the accuracy of the detection.
    keyPhrases :: Prelude.Maybe [KeyPhrase],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DetectKeyPhrasesResponse
newDetectKeyPhrasesResponse pHttpStatus_ =
  DetectKeyPhrasesResponse'
    { keyPhrases =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key phrases that Amazon Comprehend identified in the
-- input text. For each key phrase, the response provides the text of the
-- key phrase, where the key phrase begins and ends, and the level of
-- confidence that Amazon Comprehend has in the accuracy of the detection.
detectKeyPhrasesResponse_keyPhrases :: Lens.Lens' DetectKeyPhrasesResponse (Prelude.Maybe [KeyPhrase])
detectKeyPhrasesResponse_keyPhrases = Lens.lens (\DetectKeyPhrasesResponse' {keyPhrases} -> keyPhrases) (\s@DetectKeyPhrasesResponse' {} a -> s {keyPhrases = a} :: DetectKeyPhrasesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectKeyPhrasesResponse_httpStatus :: Lens.Lens' DetectKeyPhrasesResponse Prelude.Int
detectKeyPhrasesResponse_httpStatus = Lens.lens (\DetectKeyPhrasesResponse' {httpStatus} -> httpStatus) (\s@DetectKeyPhrasesResponse' {} a -> s {httpStatus = a} :: DetectKeyPhrasesResponse)

instance Prelude.NFData DetectKeyPhrasesResponse
