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
-- Module      : Amazonka.CloudTrail.PutInsightSelectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lets you enable Insights event logging by specifying the Insights
-- selectors that you want to enable on an existing trail. You also use
-- @PutInsightSelectors@ to turn off Insights event logging, by passing an
-- empty list of insight types. The valid Insights event types in this
-- release are @ApiErrorRateInsight@ and @ApiCallRateInsight@.
module Amazonka.CloudTrail.PutInsightSelectors
  ( -- * Creating a Request
    PutInsightSelectors (..),
    newPutInsightSelectors,

    -- * Request Lenses
    putInsightSelectors_trailName,
    putInsightSelectors_insightSelectors,

    -- * Destructuring the Response
    PutInsightSelectorsResponse (..),
    newPutInsightSelectorsResponse,

    -- * Response Lenses
    putInsightSelectorsResponse_insightSelectors,
    putInsightSelectorsResponse_trailARN,
    putInsightSelectorsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutInsightSelectors' smart constructor.
data PutInsightSelectors = PutInsightSelectors'
  { -- | The name of the CloudTrail trail for which you want to change or add
    -- Insights selectors.
    trailName :: Prelude.Text,
    -- | A JSON string that contains the insight types you want to log on a
    -- trail. @ApiCallRateInsight@ and @ApiErrorRateInsight@ are valid insight
    -- types.
    insightSelectors :: [InsightSelector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInsightSelectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailName', 'putInsightSelectors_trailName' - The name of the CloudTrail trail for which you want to change or add
-- Insights selectors.
--
-- 'insightSelectors', 'putInsightSelectors_insightSelectors' - A JSON string that contains the insight types you want to log on a
-- trail. @ApiCallRateInsight@ and @ApiErrorRateInsight@ are valid insight
-- types.
newPutInsightSelectors ::
  -- | 'trailName'
  Prelude.Text ->
  PutInsightSelectors
newPutInsightSelectors pTrailName_ =
  PutInsightSelectors'
    { trailName = pTrailName_,
      insightSelectors = Prelude.mempty
    }

-- | The name of the CloudTrail trail for which you want to change or add
-- Insights selectors.
putInsightSelectors_trailName :: Lens.Lens' PutInsightSelectors Prelude.Text
putInsightSelectors_trailName = Lens.lens (\PutInsightSelectors' {trailName} -> trailName) (\s@PutInsightSelectors' {} a -> s {trailName = a} :: PutInsightSelectors)

-- | A JSON string that contains the insight types you want to log on a
-- trail. @ApiCallRateInsight@ and @ApiErrorRateInsight@ are valid insight
-- types.
putInsightSelectors_insightSelectors :: Lens.Lens' PutInsightSelectors [InsightSelector]
putInsightSelectors_insightSelectors = Lens.lens (\PutInsightSelectors' {insightSelectors} -> insightSelectors) (\s@PutInsightSelectors' {} a -> s {insightSelectors = a} :: PutInsightSelectors) Prelude.. Lens.coerced

instance Core.AWSRequest PutInsightSelectors where
  type
    AWSResponse PutInsightSelectors =
      PutInsightSelectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInsightSelectorsResponse'
            Prelude.<$> ( x
                            Data..?> "InsightSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TrailARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInsightSelectors where
  hashWithSalt _salt PutInsightSelectors' {..} =
    _salt
      `Prelude.hashWithSalt` trailName
      `Prelude.hashWithSalt` insightSelectors

instance Prelude.NFData PutInsightSelectors where
  rnf PutInsightSelectors' {..} =
    Prelude.rnf trailName `Prelude.seq`
      Prelude.rnf insightSelectors

instance Data.ToHeaders PutInsightSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutInsightSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInsightSelectors where
  toJSON PutInsightSelectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TrailName" Data..= trailName),
            Prelude.Just
              ("InsightSelectors" Data..= insightSelectors)
          ]
      )

instance Data.ToPath PutInsightSelectors where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInsightSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInsightSelectorsResponse' smart constructor.
data PutInsightSelectorsResponse = PutInsightSelectorsResponse'
  { -- | A JSON string that contains the Insights event types that you want to
    -- log on a trail. The valid Insights types in this release are
    -- @ApiErrorRateInsight@ and @ApiCallRateInsight@.
    insightSelectors :: Prelude.Maybe [InsightSelector],
    -- | The Amazon Resource Name (ARN) of a trail for which you want to change
    -- or add Insights selectors.
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInsightSelectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightSelectors', 'putInsightSelectorsResponse_insightSelectors' - A JSON string that contains the Insights event types that you want to
-- log on a trail. The valid Insights types in this release are
-- @ApiErrorRateInsight@ and @ApiCallRateInsight@.
--
-- 'trailARN', 'putInsightSelectorsResponse_trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to change
-- or add Insights selectors.
--
-- 'httpStatus', 'putInsightSelectorsResponse_httpStatus' - The response's http status code.
newPutInsightSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInsightSelectorsResponse
newPutInsightSelectorsResponse pHttpStatus_ =
  PutInsightSelectorsResponse'
    { insightSelectors =
        Prelude.Nothing,
      trailARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON string that contains the Insights event types that you want to
-- log on a trail. The valid Insights types in this release are
-- @ApiErrorRateInsight@ and @ApiCallRateInsight@.
putInsightSelectorsResponse_insightSelectors :: Lens.Lens' PutInsightSelectorsResponse (Prelude.Maybe [InsightSelector])
putInsightSelectorsResponse_insightSelectors = Lens.lens (\PutInsightSelectorsResponse' {insightSelectors} -> insightSelectors) (\s@PutInsightSelectorsResponse' {} a -> s {insightSelectors = a} :: PutInsightSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a trail for which you want to change
-- or add Insights selectors.
putInsightSelectorsResponse_trailARN :: Lens.Lens' PutInsightSelectorsResponse (Prelude.Maybe Prelude.Text)
putInsightSelectorsResponse_trailARN = Lens.lens (\PutInsightSelectorsResponse' {trailARN} -> trailARN) (\s@PutInsightSelectorsResponse' {} a -> s {trailARN = a} :: PutInsightSelectorsResponse)

-- | The response's http status code.
putInsightSelectorsResponse_httpStatus :: Lens.Lens' PutInsightSelectorsResponse Prelude.Int
putInsightSelectorsResponse_httpStatus = Lens.lens (\PutInsightSelectorsResponse' {httpStatus} -> httpStatus) (\s@PutInsightSelectorsResponse' {} a -> s {httpStatus = a} :: PutInsightSelectorsResponse)

instance Prelude.NFData PutInsightSelectorsResponse where
  rnf PutInsightSelectorsResponse' {..} =
    Prelude.rnf insightSelectors `Prelude.seq`
      Prelude.rnf trailARN `Prelude.seq`
        Prelude.rnf httpStatus
