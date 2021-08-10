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
-- Module      : Network.AWS.Comprehend.DetectDominantLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text. For a list of
-- languages that Amazon Comprehend can detect, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages>.
module Network.AWS.Comprehend.DetectDominantLanguage
  ( -- * Creating a Request
    DetectDominantLanguage (..),
    newDetectDominantLanguage,

    -- * Request Lenses
    detectDominantLanguage_text,

    -- * Destructuring the Response
    DetectDominantLanguageResponse (..),
    newDetectDominantLanguageResponse,

    -- * Response Lenses
    detectDominantLanguageResponse_languages,
    detectDominantLanguageResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectDominantLanguage' smart constructor.
data DetectDominantLanguage = DetectDominantLanguage'
  { -- | A UTF-8 text string. Each string should contain at least 20 characters
    -- and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    text :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectDominantLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectDominantLanguage_text' - A UTF-8 text string. Each string should contain at least 20 characters
-- and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
newDetectDominantLanguage ::
  -- | 'text'
  Prelude.Text ->
  DetectDominantLanguage
newDetectDominantLanguage pText_ =
  DetectDominantLanguage'
    { text =
        Core._Sensitive Lens.# pText_
    }

-- | A UTF-8 text string. Each string should contain at least 20 characters
-- and must contain fewer that 5,000 bytes of UTF-8 encoded characters.
detectDominantLanguage_text :: Lens.Lens' DetectDominantLanguage Prelude.Text
detectDominantLanguage_text = Lens.lens (\DetectDominantLanguage' {text} -> text) (\s@DetectDominantLanguage' {} a -> s {text = a} :: DetectDominantLanguage) Prelude.. Core._Sensitive

instance Core.AWSRequest DetectDominantLanguage where
  type
    AWSResponse DetectDominantLanguage =
      DetectDominantLanguageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectDominantLanguageResponse'
            Prelude.<$> (x Core..?> "Languages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectDominantLanguage

instance Prelude.NFData DetectDominantLanguage

instance Core.ToHeaders DetectDominantLanguage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectDominantLanguage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectDominantLanguage where
  toJSON DetectDominantLanguage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Core..= text)]
      )

instance Core.ToPath DetectDominantLanguage where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectDominantLanguage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectDominantLanguageResponse' smart constructor.
data DetectDominantLanguageResponse = DetectDominantLanguageResponse'
  { -- | The languages that Amazon Comprehend detected in the input text. For
    -- each language, the response returns the RFC 5646 language code and the
    -- level of confidence that Amazon Comprehend has in the accuracy of its
    -- inference. For more information about RFC 5646, see
    -- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
    -- the /IETF Tools/ web site.
    languages :: Prelude.Maybe [DominantLanguage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectDominantLanguageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languages', 'detectDominantLanguageResponse_languages' - The languages that Amazon Comprehend detected in the input text. For
-- each language, the response returns the RFC 5646 language code and the
-- level of confidence that Amazon Comprehend has in the accuracy of its
-- inference. For more information about RFC 5646, see
-- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
-- the /IETF Tools/ web site.
--
-- 'httpStatus', 'detectDominantLanguageResponse_httpStatus' - The response's http status code.
newDetectDominantLanguageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectDominantLanguageResponse
newDetectDominantLanguageResponse pHttpStatus_ =
  DetectDominantLanguageResponse'
    { languages =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The languages that Amazon Comprehend detected in the input text. For
-- each language, the response returns the RFC 5646 language code and the
-- level of confidence that Amazon Comprehend has in the accuracy of its
-- inference. For more information about RFC 5646, see
-- <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on
-- the /IETF Tools/ web site.
detectDominantLanguageResponse_languages :: Lens.Lens' DetectDominantLanguageResponse (Prelude.Maybe [DominantLanguage])
detectDominantLanguageResponse_languages = Lens.lens (\DetectDominantLanguageResponse' {languages} -> languages) (\s@DetectDominantLanguageResponse' {} a -> s {languages = a} :: DetectDominantLanguageResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectDominantLanguageResponse_httpStatus :: Lens.Lens' DetectDominantLanguageResponse Prelude.Int
detectDominantLanguageResponse_httpStatus = Lens.lens (\DetectDominantLanguageResponse' {httpStatus} -> httpStatus) (\s@DetectDominantLanguageResponse' {} a -> s {httpStatus = a} :: DetectDominantLanguageResponse)

instance
  Prelude.NFData
    DetectDominantLanguageResponse
