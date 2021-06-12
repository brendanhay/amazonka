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
-- Module      : Network.AWS.CloudTrail.PutInsightSelectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lets you enable Insights event logging by specifying the Insights
-- selectors that you want to enable on an existing trail. You also use
-- @PutInsightSelectors@ to turn off Insights event logging, by passing an
-- empty list of insight types. In this release, only @ApiCallRateInsight@
-- is supported as an Insights selector.
module Network.AWS.CloudTrail.PutInsightSelectors
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
    putInsightSelectorsResponse_trailARN,
    putInsightSelectorsResponse_insightSelectors,
    putInsightSelectorsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutInsightSelectors' smart constructor.
data PutInsightSelectors = PutInsightSelectors'
  { -- | The name of the CloudTrail trail for which you want to change or add
    -- Insights selectors.
    trailName :: Core.Text,
    -- | A JSON string that contains the insight types you want to log on a
    -- trail. In this release, only @ApiCallRateInsight@ is supported as an
    -- insight type.
    insightSelectors :: [InsightSelector]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
newPutInsightSelectors ::
  -- | 'trailName'
  Core.Text ->
  PutInsightSelectors
newPutInsightSelectors pTrailName_ =
  PutInsightSelectors'
    { trailName = pTrailName_,
      insightSelectors = Core.mempty
    }

-- | The name of the CloudTrail trail for which you want to change or add
-- Insights selectors.
putInsightSelectors_trailName :: Lens.Lens' PutInsightSelectors Core.Text
putInsightSelectors_trailName = Lens.lens (\PutInsightSelectors' {trailName} -> trailName) (\s@PutInsightSelectors' {} a -> s {trailName = a} :: PutInsightSelectors)

-- | A JSON string that contains the insight types you want to log on a
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
putInsightSelectors_insightSelectors :: Lens.Lens' PutInsightSelectors [InsightSelector]
putInsightSelectors_insightSelectors = Lens.lens (\PutInsightSelectors' {insightSelectors} -> insightSelectors) (\s@PutInsightSelectors' {} a -> s {insightSelectors = a} :: PutInsightSelectors) Core.. Lens._Coerce

instance Core.AWSRequest PutInsightSelectors where
  type
    AWSResponse PutInsightSelectors =
      PutInsightSelectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutInsightSelectorsResponse'
            Core.<$> (x Core..?> "TrailARN")
            Core.<*> (x Core..?> "InsightSelectors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutInsightSelectors

instance Core.NFData PutInsightSelectors

instance Core.ToHeaders PutInsightSelectors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutInsightSelectors" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutInsightSelectors where
  toJSON PutInsightSelectors' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrailName" Core..= trailName),
            Core.Just
              ("InsightSelectors" Core..= insightSelectors)
          ]
      )

instance Core.ToPath PutInsightSelectors where
  toPath = Core.const "/"

instance Core.ToQuery PutInsightSelectors where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutInsightSelectorsResponse' smart constructor.
data PutInsightSelectorsResponse = PutInsightSelectorsResponse'
  { -- | The Amazon Resource Name (ARN) of a trail for which you want to change
    -- or add Insights selectors.
    trailARN :: Core.Maybe Core.Text,
    -- | A JSON string that contains the insight types you want to log on a
    -- trail. In this release, only @ApiCallRateInsight@ is supported as an
    -- insight type.
    insightSelectors :: Core.Maybe [InsightSelector],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutInsightSelectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'putInsightSelectorsResponse_trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to change
-- or add Insights selectors.
--
-- 'insightSelectors', 'putInsightSelectorsResponse_insightSelectors' - A JSON string that contains the insight types you want to log on a
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
--
-- 'httpStatus', 'putInsightSelectorsResponse_httpStatus' - The response's http status code.
newPutInsightSelectorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutInsightSelectorsResponse
newPutInsightSelectorsResponse pHttpStatus_ =
  PutInsightSelectorsResponse'
    { trailARN =
        Core.Nothing,
      insightSelectors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of a trail for which you want to change
-- or add Insights selectors.
putInsightSelectorsResponse_trailARN :: Lens.Lens' PutInsightSelectorsResponse (Core.Maybe Core.Text)
putInsightSelectorsResponse_trailARN = Lens.lens (\PutInsightSelectorsResponse' {trailARN} -> trailARN) (\s@PutInsightSelectorsResponse' {} a -> s {trailARN = a} :: PutInsightSelectorsResponse)

-- | A JSON string that contains the insight types you want to log on a
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
putInsightSelectorsResponse_insightSelectors :: Lens.Lens' PutInsightSelectorsResponse (Core.Maybe [InsightSelector])
putInsightSelectorsResponse_insightSelectors = Lens.lens (\PutInsightSelectorsResponse' {insightSelectors} -> insightSelectors) (\s@PutInsightSelectorsResponse' {} a -> s {insightSelectors = a} :: PutInsightSelectorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putInsightSelectorsResponse_httpStatus :: Lens.Lens' PutInsightSelectorsResponse Core.Int
putInsightSelectorsResponse_httpStatus = Lens.lens (\PutInsightSelectorsResponse' {httpStatus} -> httpStatus) (\s@PutInsightSelectorsResponse' {} a -> s {httpStatus = a} :: PutInsightSelectorsResponse)

instance Core.NFData PutInsightSelectorsResponse
