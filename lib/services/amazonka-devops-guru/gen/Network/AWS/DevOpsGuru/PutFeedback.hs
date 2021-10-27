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
-- Module      : Network.AWS.DevOpsGuru.PutFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Collects customer feedback about the specified insight.
module Network.AWS.DevOpsGuru.PutFeedback
  ( -- * Creating a Request
    PutFeedback (..),
    newPutFeedback,

    -- * Request Lenses
    putFeedback_insightFeedback,

    -- * Destructuring the Response
    PutFeedbackResponse (..),
    newPutFeedbackResponse,

    -- * Response Lenses
    putFeedbackResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFeedback' smart constructor.
data PutFeedback = PutFeedback'
  { -- | The feedback from customers is about the recommendations in this
    -- insight.
    insightFeedback :: Prelude.Maybe InsightFeedback
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightFeedback', 'putFeedback_insightFeedback' - The feedback from customers is about the recommendations in this
-- insight.
newPutFeedback ::
  PutFeedback
newPutFeedback =
  PutFeedback' {insightFeedback = Prelude.Nothing}

-- | The feedback from customers is about the recommendations in this
-- insight.
putFeedback_insightFeedback :: Lens.Lens' PutFeedback (Prelude.Maybe InsightFeedback)
putFeedback_insightFeedback = Lens.lens (\PutFeedback' {insightFeedback} -> insightFeedback) (\s@PutFeedback' {} a -> s {insightFeedback = a} :: PutFeedback)

instance Core.AWSRequest PutFeedback where
  type AWSResponse PutFeedback = PutFeedbackResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutFeedback

instance Prelude.NFData PutFeedback

instance Core.ToHeaders PutFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutFeedback where
  toJSON PutFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InsightFeedback" Core..=)
              Prelude.<$> insightFeedback
          ]
      )

instance Core.ToPath PutFeedback where
  toPath = Prelude.const "/feedback"

instance Core.ToQuery PutFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFeedbackResponse' smart constructor.
data PutFeedbackResponse = PutFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFeedbackResponse_httpStatus' - The response's http status code.
newPutFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutFeedbackResponse
newPutFeedbackResponse pHttpStatus_ =
  PutFeedbackResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putFeedbackResponse_httpStatus :: Lens.Lens' PutFeedbackResponse Prelude.Int
putFeedbackResponse_httpStatus = Lens.lens (\PutFeedbackResponse' {httpStatus} -> httpStatus) (\s@PutFeedbackResponse' {} a -> s {httpStatus = a} :: PutFeedbackResponse)

instance Prelude.NFData PutFeedbackResponse
