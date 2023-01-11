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
-- Module      : Amazonka.CloudTrail.GetInsightSelectors
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- in the /CloudTrail User Guide/.
module Amazonka.CloudTrail.GetInsightSelectors
  ( -- * Creating a Request
    GetInsightSelectors (..),
    newGetInsightSelectors,

    -- * Request Lenses
    getInsightSelectors_trailName,

    -- * Destructuring the Response
    GetInsightSelectorsResponse (..),
    newGetInsightSelectorsResponse,

    -- * Response Lenses
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSelectorsResponse'
            Prelude.<$> ( x Data..?> "InsightSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TrailARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightSelectors where
  hashWithSalt _salt GetInsightSelectors' {..} =
    _salt `Prelude.hashWithSalt` trailName

instance Prelude.NFData GetInsightSelectors where
  rnf GetInsightSelectors' {..} = Prelude.rnf trailName

instance Data.ToHeaders GetInsightSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetInsightSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInsightSelectors where
  toJSON GetInsightSelectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrailName" Data..= trailName)]
      )

instance Data.ToPath GetInsightSelectors where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInsightSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightSelectorsResponse' smart constructor.
data GetInsightSelectorsResponse = GetInsightSelectorsResponse'
  { -- | A JSON string that contains the insight types you want to log on a
    -- trail. In this release, @ApiErrorRateInsight@ and @ApiCallRateInsight@
    -- are supported as insight types.
    insightSelectors :: Prelude.Maybe [InsightSelector],
    -- | The Amazon Resource Name (ARN) of a trail for which you want to get
    -- Insights selectors.
    trailARN :: Prelude.Maybe Prelude.Text,
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
-- 'insightSelectors', 'getInsightSelectorsResponse_insightSelectors' - A JSON string that contains the insight types you want to log on a
-- trail. In this release, @ApiErrorRateInsight@ and @ApiCallRateInsight@
-- are supported as insight types.
--
-- 'trailARN', 'getInsightSelectorsResponse_trailARN' - The Amazon Resource Name (ARN) of a trail for which you want to get
-- Insights selectors.
--
-- 'httpStatus', 'getInsightSelectorsResponse_httpStatus' - The response's http status code.
newGetInsightSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightSelectorsResponse
newGetInsightSelectorsResponse pHttpStatus_ =
  GetInsightSelectorsResponse'
    { insightSelectors =
        Prelude.Nothing,
      trailARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A JSON string that contains the insight types you want to log on a
-- trail. In this release, @ApiErrorRateInsight@ and @ApiCallRateInsight@
-- are supported as insight types.
getInsightSelectorsResponse_insightSelectors :: Lens.Lens' GetInsightSelectorsResponse (Prelude.Maybe [InsightSelector])
getInsightSelectorsResponse_insightSelectors = Lens.lens (\GetInsightSelectorsResponse' {insightSelectors} -> insightSelectors) (\s@GetInsightSelectorsResponse' {} a -> s {insightSelectors = a} :: GetInsightSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a trail for which you want to get
-- Insights selectors.
getInsightSelectorsResponse_trailARN :: Lens.Lens' GetInsightSelectorsResponse (Prelude.Maybe Prelude.Text)
getInsightSelectorsResponse_trailARN = Lens.lens (\GetInsightSelectorsResponse' {trailARN} -> trailARN) (\s@GetInsightSelectorsResponse' {} a -> s {trailARN = a} :: GetInsightSelectorsResponse)

-- | The response's http status code.
getInsightSelectorsResponse_httpStatus :: Lens.Lens' GetInsightSelectorsResponse Prelude.Int
getInsightSelectorsResponse_httpStatus = Lens.lens (\GetInsightSelectorsResponse' {httpStatus} -> httpStatus) (\s@GetInsightSelectorsResponse' {} a -> s {httpStatus = a} :: GetInsightSelectorsResponse)

instance Prelude.NFData GetInsightSelectorsResponse where
  rnf GetInsightSelectorsResponse' {..} =
    Prelude.rnf insightSelectors
      `Prelude.seq` Prelude.rnf trailARN
      `Prelude.seq` Prelude.rnf httpStatus
