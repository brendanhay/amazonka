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
-- Module      : Network.AWS.CloudTrail.GetInsightSelectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the Insights event selectors that you
-- configured for your trail. @GetInsightSelectors@ shows if CloudTrail
-- Insights event logging is enabled on the trail, and if it is, which
-- insight types are enabled. If you run @GetInsightSelectors@ on a trail
-- that does not have Insights events enabled, the operation throws the
-- exception @InsightNotEnabledException@
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-insights-events-with-cloudtrail.html Logging CloudTrail Insights Events for Trails>
-- in the /AWS CloudTrail User Guide/.
module Network.AWS.CloudTrail.GetInsightSelectors
  ( -- * Creating a Request
    GetInsightSelectors (..),
    newGetInsightSelectors,

    -- * Request Lenses
    getInsightSelectors_trailName,

    -- * Destructuring the Response
    GetInsightSelectorsResponse (..),
    newGetInsightSelectorsResponse,

    -- * Response Lenses
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInsightSelectors' smart constructor.
data GetInsightSelectors = GetInsightSelectors'
  { -- | Specifies the name of the trail or trail ARN. If you specify a trail
    -- name, the string must meet the following requirements:
    --
    -- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
    --     underscores (_), or dashes (-)
    --
    -- -   Start with a letter or number, and end with a letter or number
    --
    -- -   Be between 3 and 128 characters
    --
    -- -   Have no adjacent periods, underscores or dashes. Names like
    --     @my-_namespace@ and @my--namespace@ are not valid.
    --
    -- -   Not be in IP address format (for example, 192.168.5.4)
    --
    -- If you specify a trail ARN, it must be in the format:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightSelectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailName', 'getInsightSelectors_trailName' - Specifies the name of the trail or trail ARN. If you specify a trail
-- name, the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newGetInsightSelectors ::
  -- | 'trailName'
  Prelude.Text ->
  GetInsightSelectors
newGetInsightSelectors pTrailName_ =
  GetInsightSelectors' {trailName = pTrailName_}

-- | Specifies the name of the trail or trail ARN. If you specify a trail
-- name, the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
getInsightSelectors_trailName :: Lens.Lens' GetInsightSelectors Prelude.Text
getInsightSelectors_trailName = Lens.lens (\GetInsightSelectors' {trailName} -> trailName) (\s@GetInsightSelectors' {} a -> s {trailName = a} :: GetInsightSelectors)

instance Core.AWSRequest GetInsightSelectors where
  type
    AWSResponse GetInsightSelectors =
      GetInsightSelectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSelectorsResponse'
            Prelude.<$> (x Core..?> "TrailARN")
            Prelude.<*> ( x Core..?> "InsightSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightSelectors

instance Prelude.NFData GetInsightSelectors

instance Core.ToHeaders GetInsightSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetInsightSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInsightSelectors where
  toJSON GetInsightSelectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrailName" Core..= trailName)]
      )

instance Core.ToPath GetInsightSelectors where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInsightSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightSelectorsResponse' smart constructor.
data GetInsightSelectorsResponse = GetInsightSelectorsResponse'
  { -- | The Amazon Resource Name (ARN) of a trail for which you want to get
    -- Insights selectors.
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | A JSON string that contains the insight types you want to log on a
    -- trail. In this release, only @ApiCallRateInsight@ is supported as an
    -- insight type.
    insightSelectors :: Prelude.Maybe [InsightSelector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightSelectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'getInsightSelectorsResponse_trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to get
-- Insights selectors.
--
-- 'insightSelectors', 'getInsightSelectorsResponse_insightSelectors' - A JSON string that contains the insight types you want to log on a
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
--
-- 'httpStatus', 'getInsightSelectorsResponse_httpStatus' - The response's http status code.
newGetInsightSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightSelectorsResponse
newGetInsightSelectorsResponse pHttpStatus_ =
  GetInsightSelectorsResponse'
    { trailARN =
        Prelude.Nothing,
      insightSelectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of a trail for which you want to get
-- Insights selectors.
getInsightSelectorsResponse_trailARN :: Lens.Lens' GetInsightSelectorsResponse (Prelude.Maybe Prelude.Text)
getInsightSelectorsResponse_trailARN = Lens.lens (\GetInsightSelectorsResponse' {trailARN} -> trailARN) (\s@GetInsightSelectorsResponse' {} a -> s {trailARN = a} :: GetInsightSelectorsResponse)

-- | A JSON string that contains the insight types you want to log on a
-- trail. In this release, only @ApiCallRateInsight@ is supported as an
-- insight type.
getInsightSelectorsResponse_insightSelectors :: Lens.Lens' GetInsightSelectorsResponse (Prelude.Maybe [InsightSelector])
getInsightSelectorsResponse_insightSelectors = Lens.lens (\GetInsightSelectorsResponse' {insightSelectors} -> insightSelectors) (\s@GetInsightSelectorsResponse' {} a -> s {insightSelectors = a} :: GetInsightSelectorsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getInsightSelectorsResponse_httpStatus :: Lens.Lens' GetInsightSelectorsResponse Prelude.Int
getInsightSelectorsResponse_httpStatus = Lens.lens (\GetInsightSelectorsResponse' {httpStatus} -> httpStatus) (\s@GetInsightSelectorsResponse' {} a -> s {httpStatus = a} :: GetInsightSelectorsResponse)

instance Prelude.NFData GetInsightSelectorsResponse
