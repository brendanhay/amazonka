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
-- Module      : Network.AWS.Comprehend.DetectSentiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text and returns an inference of the prevailing sentiment
-- (@POSITIVE@, @NEUTRAL@, @MIXED@, or @NEGATIVE@).
module Network.AWS.Comprehend.DetectSentiment
  ( -- * Creating a Request
    DetectSentiment (..),
    newDetectSentiment,

    -- * Request Lenses
    detectSentiment_text,
    detectSentiment_languageCode,

    -- * Destructuring the Response
    DetectSentimentResponse (..),
    newDetectSentimentResponse,

    -- * Response Lenses
    detectSentimentResponse_sentimentScore,
    detectSentimentResponse_sentiment,
    detectSentimentResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
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
-- Create a value of 'DetectSentiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectSentiment_text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
--
-- 'languageCode', 'detectSentiment_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newDetectSentiment ::
  -- | 'text'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectSentiment
newDetectSentiment pText_ pLanguageCode_ =
  DetectSentiment'
    { text =
        Core._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
detectSentiment_text :: Lens.Lens' DetectSentiment Core.Text
detectSentiment_text = Lens.lens (\DetectSentiment' {text} -> text) (\s@DetectSentiment' {} a -> s {text = a} :: DetectSentiment) Core.. Core._Sensitive

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
detectSentiment_languageCode :: Lens.Lens' DetectSentiment LanguageCode
detectSentiment_languageCode = Lens.lens (\DetectSentiment' {languageCode} -> languageCode) (\s@DetectSentiment' {} a -> s {languageCode = a} :: DetectSentiment)

instance Core.AWSRequest DetectSentiment where
  type
    AWSResponse DetectSentiment =
      DetectSentimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectSentimentResponse'
            Core.<$> (x Core..?> "SentimentScore")
            Core.<*> (x Core..?> "Sentiment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetectSentiment

instance Core.NFData DetectSentiment

instance Core.ToHeaders DetectSentiment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectSentiment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetectSentiment where
  toJSON DetectSentiment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectSentiment where
  toPath = Core.const "/"

instance Core.ToQuery DetectSentiment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { -- | An object that lists the sentiments, and their corresponding confidence
    -- levels.
    sentimentScore :: Core.Maybe SentimentScore,
    -- | The inferred sentiment that Amazon Comprehend has the highest level of
    -- confidence in.
    sentiment :: Core.Maybe SentimentType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectSentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentScore', 'detectSentimentResponse_sentimentScore' - An object that lists the sentiments, and their corresponding confidence
-- levels.
--
-- 'sentiment', 'detectSentimentResponse_sentiment' - The inferred sentiment that Amazon Comprehend has the highest level of
-- confidence in.
--
-- 'httpStatus', 'detectSentimentResponse_httpStatus' - The response's http status code.
newDetectSentimentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetectSentimentResponse
newDetectSentimentResponse pHttpStatus_ =
  DetectSentimentResponse'
    { sentimentScore =
        Core.Nothing,
      sentiment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that lists the sentiments, and their corresponding confidence
-- levels.
detectSentimentResponse_sentimentScore :: Lens.Lens' DetectSentimentResponse (Core.Maybe SentimentScore)
detectSentimentResponse_sentimentScore = Lens.lens (\DetectSentimentResponse' {sentimentScore} -> sentimentScore) (\s@DetectSentimentResponse' {} a -> s {sentimentScore = a} :: DetectSentimentResponse)

-- | The inferred sentiment that Amazon Comprehend has the highest level of
-- confidence in.
detectSentimentResponse_sentiment :: Lens.Lens' DetectSentimentResponse (Core.Maybe SentimentType)
detectSentimentResponse_sentiment = Lens.lens (\DetectSentimentResponse' {sentiment} -> sentiment) (\s@DetectSentimentResponse' {} a -> s {sentiment = a} :: DetectSentimentResponse)

-- | The response's http status code.
detectSentimentResponse_httpStatus :: Lens.Lens' DetectSentimentResponse Core.Int
detectSentimentResponse_httpStatus = Lens.lens (\DetectSentimentResponse' {httpStatus} -> httpStatus) (\s@DetectSentimentResponse' {} a -> s {httpStatus = a} :: DetectSentimentResponse)

instance Core.NFData DetectSentimentResponse
