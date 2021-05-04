{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectSentiment' smart constructor.
data DetectSentiment = DetectSentiment'
  { -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
    -- UTF-8 encoded characters.
    text :: Prelude.Sensitive Prelude.Text,
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectSentiment
newDetectSentiment pText_ pLanguageCode_ =
  DetectSentiment'
    { text =
        Prelude._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
detectSentiment_text :: Lens.Lens' DetectSentiment Prelude.Text
detectSentiment_text = Lens.lens (\DetectSentiment' {text} -> text) (\s@DetectSentiment' {} a -> s {text = a} :: DetectSentiment) Prelude.. Prelude._Sensitive

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
detectSentiment_languageCode :: Lens.Lens' DetectSentiment LanguageCode
detectSentiment_languageCode = Lens.lens (\DetectSentiment' {languageCode} -> languageCode) (\s@DetectSentiment' {} a -> s {languageCode = a} :: DetectSentiment)

instance Prelude.AWSRequest DetectSentiment where
  type Rs DetectSentiment = DetectSentimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectSentimentResponse'
            Prelude.<$> (x Prelude..?> "SentimentScore")
            Prelude.<*> (x Prelude..?> "Sentiment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectSentiment

instance Prelude.NFData DetectSentiment

instance Prelude.ToHeaders DetectSentiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DetectSentiment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DetectSentiment where
  toJSON DetectSentiment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Prelude..= text),
            Prelude.Just
              ("LanguageCode" Prelude..= languageCode)
          ]
      )

instance Prelude.ToPath DetectSentiment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetectSentiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectSentimentResponse' smart constructor.
data DetectSentimentResponse = DetectSentimentResponse'
  { -- | An object that lists the sentiments, and their corresponding confidence
    -- levels.
    sentimentScore :: Prelude.Maybe SentimentScore,
    -- | The inferred sentiment that Amazon Comprehend has the highest level of
    -- confidence in.
    sentiment :: Prelude.Maybe SentimentType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DetectSentimentResponse
newDetectSentimentResponse pHttpStatus_ =
  DetectSentimentResponse'
    { sentimentScore =
        Prelude.Nothing,
      sentiment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that lists the sentiments, and their corresponding confidence
-- levels.
detectSentimentResponse_sentimentScore :: Lens.Lens' DetectSentimentResponse (Prelude.Maybe SentimentScore)
detectSentimentResponse_sentimentScore = Lens.lens (\DetectSentimentResponse' {sentimentScore} -> sentimentScore) (\s@DetectSentimentResponse' {} a -> s {sentimentScore = a} :: DetectSentimentResponse)

-- | The inferred sentiment that Amazon Comprehend has the highest level of
-- confidence in.
detectSentimentResponse_sentiment :: Lens.Lens' DetectSentimentResponse (Prelude.Maybe SentimentType)
detectSentimentResponse_sentiment = Lens.lens (\DetectSentimentResponse' {sentiment} -> sentiment) (\s@DetectSentimentResponse' {} a -> s {sentiment = a} :: DetectSentimentResponse)

-- | The response's http status code.
detectSentimentResponse_httpStatus :: Lens.Lens' DetectSentimentResponse Prelude.Int
detectSentimentResponse_httpStatus = Lens.lens (\DetectSentimentResponse' {httpStatus} -> httpStatus) (\s@DetectSentimentResponse' {} a -> s {httpStatus = a} :: DetectSentimentResponse)

instance Prelude.NFData DetectSentimentResponse
