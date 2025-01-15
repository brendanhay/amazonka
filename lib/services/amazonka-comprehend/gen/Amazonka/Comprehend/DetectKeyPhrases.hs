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
-- Module      : Amazonka.Comprehend.DetectKeyPhrases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in the text.
module Amazonka.Comprehend.DetectKeyPhrases
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectKeyPhrases' smart constructor.
data DetectKeyPhrases = DetectKeyPhrases'
  { -- | A UTF-8 text string. The string must contain less than 100 KB of UTF-8
    -- encoded characters.
    text :: Data.Sensitive Prelude.Text,
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
-- 'text', 'detectKeyPhrases_text' - A UTF-8 text string. The string must contain less than 100 KB of UTF-8
-- encoded characters.
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
        Data._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. The string must contain less than 100 KB of UTF-8
-- encoded characters.
detectKeyPhrases_text :: Lens.Lens' DetectKeyPhrases Prelude.Text
detectKeyPhrases_text = Lens.lens (\DetectKeyPhrases' {text} -> text) (\s@DetectKeyPhrases' {} a -> s {text = a} :: DetectKeyPhrases) Prelude.. Data._Sensitive

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
detectKeyPhrases_languageCode :: Lens.Lens' DetectKeyPhrases LanguageCode
detectKeyPhrases_languageCode = Lens.lens (\DetectKeyPhrases' {languageCode} -> languageCode) (\s@DetectKeyPhrases' {} a -> s {languageCode = a} :: DetectKeyPhrases)

instance Core.AWSRequest DetectKeyPhrases where
  type
    AWSResponse DetectKeyPhrases =
      DetectKeyPhrasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectKeyPhrasesResponse'
            Prelude.<$> (x Data..?> "KeyPhrases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectKeyPhrases where
  hashWithSalt _salt DetectKeyPhrases' {..} =
    _salt
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData DetectKeyPhrases where
  rnf DetectKeyPhrases' {..} =
    Prelude.rnf text `Prelude.seq`
      Prelude.rnf languageCode

instance Data.ToHeaders DetectKeyPhrases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DetectKeyPhrases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectKeyPhrases where
  toJSON DetectKeyPhrases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Data..= text),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath DetectKeyPhrases where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectKeyPhrases where
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
detectKeyPhrasesResponse_keyPhrases = Lens.lens (\DetectKeyPhrasesResponse' {keyPhrases} -> keyPhrases) (\s@DetectKeyPhrasesResponse' {} a -> s {keyPhrases = a} :: DetectKeyPhrasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectKeyPhrasesResponse_httpStatus :: Lens.Lens' DetectKeyPhrasesResponse Prelude.Int
detectKeyPhrasesResponse_httpStatus = Lens.lens (\DetectKeyPhrasesResponse' {httpStatus} -> httpStatus) (\s@DetectKeyPhrasesResponse' {} a -> s {httpStatus = a} :: DetectKeyPhrasesResponse)

instance Prelude.NFData DetectKeyPhrasesResponse where
  rnf DetectKeyPhrasesResponse' {..} =
    Prelude.rnf keyPhrases `Prelude.seq`
      Prelude.rnf httpStatus
