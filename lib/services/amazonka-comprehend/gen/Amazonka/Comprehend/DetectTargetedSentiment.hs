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
-- Module      : Amazonka.Comprehend.DetectTargetedSentiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the input text and returns a sentiment analysis for each entity
-- identified in the text.
--
-- For more information about targeted sentiment, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html Targeted sentiment>.
module Amazonka.Comprehend.DetectTargetedSentiment
  ( -- * Creating a Request
    DetectTargetedSentiment (..),
    newDetectTargetedSentiment,

    -- * Request Lenses
    detectTargetedSentiment_text,
    detectTargetedSentiment_languageCode,

    -- * Destructuring the Response
    DetectTargetedSentimentResponse (..),
    newDetectTargetedSentimentResponse,

    -- * Response Lenses
    detectTargetedSentimentResponse_entities,
    detectTargetedSentimentResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectTargetedSentiment' smart constructor.
data DetectTargetedSentiment = DetectTargetedSentiment'
  { -- | A UTF-8 text string. The maximum string length is 5 KB.
    text :: Core.Sensitive Prelude.Text,
    -- | The language of the input documents. Currently, English is the only
    -- supported language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectTargetedSentiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectTargetedSentiment_text' - A UTF-8 text string. The maximum string length is 5 KB.
--
-- 'languageCode', 'detectTargetedSentiment_languageCode' - The language of the input documents. Currently, English is the only
-- supported language.
newDetectTargetedSentiment ::
  -- | 'text'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectTargetedSentiment
newDetectTargetedSentiment pText_ pLanguageCode_ =
  DetectTargetedSentiment'
    { text =
        Core._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. The maximum string length is 5 KB.
detectTargetedSentiment_text :: Lens.Lens' DetectTargetedSentiment Prelude.Text
detectTargetedSentiment_text = Lens.lens (\DetectTargetedSentiment' {text} -> text) (\s@DetectTargetedSentiment' {} a -> s {text = a} :: DetectTargetedSentiment) Prelude.. Core._Sensitive

-- | The language of the input documents. Currently, English is the only
-- supported language.
detectTargetedSentiment_languageCode :: Lens.Lens' DetectTargetedSentiment LanguageCode
detectTargetedSentiment_languageCode = Lens.lens (\DetectTargetedSentiment' {languageCode} -> languageCode) (\s@DetectTargetedSentiment' {} a -> s {languageCode = a} :: DetectTargetedSentiment)

instance Core.AWSRequest DetectTargetedSentiment where
  type
    AWSResponse DetectTargetedSentiment =
      DetectTargetedSentimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectTargetedSentimentResponse'
            Prelude.<$> (x Core..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectTargetedSentiment where
  hashWithSalt _salt DetectTargetedSentiment' {..} =
    _salt `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData DetectTargetedSentiment where
  rnf DetectTargetedSentiment' {..} =
    Prelude.rnf text
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders DetectTargetedSentiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectTargetedSentiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectTargetedSentiment where
  toJSON DetectTargetedSentiment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Core..= text),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectTargetedSentiment where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectTargetedSentiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectTargetedSentimentResponse' smart constructor.
data DetectTargetedSentimentResponse = DetectTargetedSentimentResponse'
  { -- | Targeted sentiment analysis for each of the entities identified in the
    -- input text.
    entities :: Prelude.Maybe [TargetedSentimentEntity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectTargetedSentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'detectTargetedSentimentResponse_entities' - Targeted sentiment analysis for each of the entities identified in the
-- input text.
--
-- 'httpStatus', 'detectTargetedSentimentResponse_httpStatus' - The response's http status code.
newDetectTargetedSentimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectTargetedSentimentResponse
newDetectTargetedSentimentResponse pHttpStatus_ =
  DetectTargetedSentimentResponse'
    { entities =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Targeted sentiment analysis for each of the entities identified in the
-- input text.
detectTargetedSentimentResponse_entities :: Lens.Lens' DetectTargetedSentimentResponse (Prelude.Maybe [TargetedSentimentEntity])
detectTargetedSentimentResponse_entities = Lens.lens (\DetectTargetedSentimentResponse' {entities} -> entities) (\s@DetectTargetedSentimentResponse' {} a -> s {entities = a} :: DetectTargetedSentimentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectTargetedSentimentResponse_httpStatus :: Lens.Lens' DetectTargetedSentimentResponse Prelude.Int
detectTargetedSentimentResponse_httpStatus = Lens.lens (\DetectTargetedSentimentResponse' {httpStatus} -> httpStatus) (\s@DetectTargetedSentimentResponse' {} a -> s {httpStatus = a} :: DetectTargetedSentimentResponse)

instance
  Prelude.NFData
    DetectTargetedSentimentResponse
  where
  rnf DetectTargetedSentimentResponse' {..} =
    Prelude.rnf entities
      `Prelude.seq` Prelude.rnf httpStatus
