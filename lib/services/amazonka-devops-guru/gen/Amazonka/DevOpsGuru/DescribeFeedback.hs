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
-- Module      : Amazonka.DevOpsGuru.DescribeFeedback
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most recent feedback submitted in the current Amazon Web
-- Services account and Region.
module Amazonka.DevOpsGuru.DescribeFeedback
  ( -- * Creating a Request
    DescribeFeedback (..),
    newDescribeFeedback,

    -- * Request Lenses
    describeFeedback_insightId,

    -- * Destructuring the Response
    DescribeFeedbackResponse (..),
    newDescribeFeedbackResponse,

    -- * Response Lenses
    describeFeedbackResponse_insightFeedback,
    describeFeedbackResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFeedback' smart constructor.
data DescribeFeedback = DescribeFeedback'
  { -- | The ID of the insight for which the feedback was provided.
    insightId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightId', 'describeFeedback_insightId' - The ID of the insight for which the feedback was provided.
newDescribeFeedback ::
  DescribeFeedback
newDescribeFeedback =
  DescribeFeedback' {insightId = Prelude.Nothing}

-- | The ID of the insight for which the feedback was provided.
describeFeedback_insightId :: Lens.Lens' DescribeFeedback (Prelude.Maybe Prelude.Text)
describeFeedback_insightId = Lens.lens (\DescribeFeedback' {insightId} -> insightId) (\s@DescribeFeedback' {} a -> s {insightId = a} :: DescribeFeedback)

instance Core.AWSRequest DescribeFeedback where
  type
    AWSResponse DescribeFeedback =
      DescribeFeedbackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFeedbackResponse'
            Prelude.<$> (x Core..?> "InsightFeedback")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFeedback where
  hashWithSalt _salt DescribeFeedback' {..} =
    _salt `Prelude.hashWithSalt` insightId

instance Prelude.NFData DescribeFeedback where
  rnf DescribeFeedback' {..} = Prelude.rnf insightId

instance Core.ToHeaders DescribeFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFeedback where
  toJSON DescribeFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [("InsightId" Core..=) Prelude.<$> insightId]
      )

instance Core.ToPath DescribeFeedback where
  toPath = Prelude.const "/feedback"

instance Core.ToQuery DescribeFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFeedbackResponse' smart constructor.
data DescribeFeedbackResponse = DescribeFeedbackResponse'
  { insightFeedback :: Prelude.Maybe InsightFeedback,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightFeedback', 'describeFeedbackResponse_insightFeedback' - Undocumented member.
--
-- 'httpStatus', 'describeFeedbackResponse_httpStatus' - The response's http status code.
newDescribeFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFeedbackResponse
newDescribeFeedbackResponse pHttpStatus_ =
  DescribeFeedbackResponse'
    { insightFeedback =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeFeedbackResponse_insightFeedback :: Lens.Lens' DescribeFeedbackResponse (Prelude.Maybe InsightFeedback)
describeFeedbackResponse_insightFeedback = Lens.lens (\DescribeFeedbackResponse' {insightFeedback} -> insightFeedback) (\s@DescribeFeedbackResponse' {} a -> s {insightFeedback = a} :: DescribeFeedbackResponse)

-- | The response's http status code.
describeFeedbackResponse_httpStatus :: Lens.Lens' DescribeFeedbackResponse Prelude.Int
describeFeedbackResponse_httpStatus = Lens.lens (\DescribeFeedbackResponse' {httpStatus} -> httpStatus) (\s@DescribeFeedbackResponse' {} a -> s {httpStatus = a} :: DescribeFeedbackResponse)

instance Prelude.NFData DescribeFeedbackResponse where
  rnf DescribeFeedbackResponse' {..} =
    Prelude.rnf insightFeedback
      `Prelude.seq` Prelude.rnf httpStatus
