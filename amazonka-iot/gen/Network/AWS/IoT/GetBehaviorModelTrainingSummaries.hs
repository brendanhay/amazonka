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
-- Module      : Network.AWS.IoT.GetBehaviorModelTrainingSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a Device Defender\'s ML Detect Security Profile training
-- model\'s status.
--
-- This operation returns paginated results.
module Network.AWS.IoT.GetBehaviorModelTrainingSummaries
  ( -- * Creating a Request
    GetBehaviorModelTrainingSummaries (..),
    newGetBehaviorModelTrainingSummaries,

    -- * Request Lenses
    getBehaviorModelTrainingSummaries_nextToken,
    getBehaviorModelTrainingSummaries_maxResults,
    getBehaviorModelTrainingSummaries_securityProfileName,

    -- * Destructuring the Response
    GetBehaviorModelTrainingSummariesResponse (..),
    newGetBehaviorModelTrainingSummariesResponse,

    -- * Response Lenses
    getBehaviorModelTrainingSummariesResponse_nextToken,
    getBehaviorModelTrainingSummariesResponse_summaries,
    getBehaviorModelTrainingSummariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBehaviorModelTrainingSummaries' smart constructor.
data GetBehaviorModelTrainingSummaries = GetBehaviorModelTrainingSummaries'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the security profile.
    securityProfileName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBehaviorModelTrainingSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBehaviorModelTrainingSummaries_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'getBehaviorModelTrainingSummaries_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'securityProfileName', 'getBehaviorModelTrainingSummaries_securityProfileName' - The name of the security profile.
newGetBehaviorModelTrainingSummaries ::
  GetBehaviorModelTrainingSummaries
newGetBehaviorModelTrainingSummaries =
  GetBehaviorModelTrainingSummaries'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      securityProfileName = Core.Nothing
    }

-- | The token for the next set of results.
getBehaviorModelTrainingSummaries_nextToken :: Lens.Lens' GetBehaviorModelTrainingSummaries (Core.Maybe Core.Text)
getBehaviorModelTrainingSummaries_nextToken = Lens.lens (\GetBehaviorModelTrainingSummaries' {nextToken} -> nextToken) (\s@GetBehaviorModelTrainingSummaries' {} a -> s {nextToken = a} :: GetBehaviorModelTrainingSummaries)

-- | The maximum number of results to return at one time. The default is 25.
getBehaviorModelTrainingSummaries_maxResults :: Lens.Lens' GetBehaviorModelTrainingSummaries (Core.Maybe Core.Natural)
getBehaviorModelTrainingSummaries_maxResults = Lens.lens (\GetBehaviorModelTrainingSummaries' {maxResults} -> maxResults) (\s@GetBehaviorModelTrainingSummaries' {} a -> s {maxResults = a} :: GetBehaviorModelTrainingSummaries)

-- | The name of the security profile.
getBehaviorModelTrainingSummaries_securityProfileName :: Lens.Lens' GetBehaviorModelTrainingSummaries (Core.Maybe Core.Text)
getBehaviorModelTrainingSummaries_securityProfileName = Lens.lens (\GetBehaviorModelTrainingSummaries' {securityProfileName} -> securityProfileName) (\s@GetBehaviorModelTrainingSummaries' {} a -> s {securityProfileName = a} :: GetBehaviorModelTrainingSummaries)

instance
  Core.AWSPager
    GetBehaviorModelTrainingSummaries
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBehaviorModelTrainingSummariesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getBehaviorModelTrainingSummariesResponse_summaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getBehaviorModelTrainingSummaries_nextToken
          Lens..~ rs
          Lens.^? getBehaviorModelTrainingSummariesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetBehaviorModelTrainingSummaries
  where
  type
    AWSResponse GetBehaviorModelTrainingSummaries =
      GetBehaviorModelTrainingSummariesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBehaviorModelTrainingSummariesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "summaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetBehaviorModelTrainingSummaries

instance
  Core.NFData
    GetBehaviorModelTrainingSummaries

instance
  Core.ToHeaders
    GetBehaviorModelTrainingSummaries
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetBehaviorModelTrainingSummaries
  where
  toPath =
    Core.const "/behavior-model-training/summaries"

instance
  Core.ToQuery
    GetBehaviorModelTrainingSummaries
  where
  toQuery GetBehaviorModelTrainingSummaries' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "securityProfileName" Core.=: securityProfileName
      ]

-- | /See:/ 'newGetBehaviorModelTrainingSummariesResponse' smart constructor.
data GetBehaviorModelTrainingSummariesResponse = GetBehaviorModelTrainingSummariesResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of all ML Detect behaviors and their model status for a given
    -- Security Profile.
    summaries :: Core.Maybe [BehaviorModelTrainingSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBehaviorModelTrainingSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getBehaviorModelTrainingSummariesResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'summaries', 'getBehaviorModelTrainingSummariesResponse_summaries' - A list of all ML Detect behaviors and their model status for a given
-- Security Profile.
--
-- 'httpStatus', 'getBehaviorModelTrainingSummariesResponse_httpStatus' - The response's http status code.
newGetBehaviorModelTrainingSummariesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBehaviorModelTrainingSummariesResponse
newGetBehaviorModelTrainingSummariesResponse
  pHttpStatus_ =
    GetBehaviorModelTrainingSummariesResponse'
      { nextToken =
          Core.Nothing,
        summaries = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
getBehaviorModelTrainingSummariesResponse_nextToken :: Lens.Lens' GetBehaviorModelTrainingSummariesResponse (Core.Maybe Core.Text)
getBehaviorModelTrainingSummariesResponse_nextToken = Lens.lens (\GetBehaviorModelTrainingSummariesResponse' {nextToken} -> nextToken) (\s@GetBehaviorModelTrainingSummariesResponse' {} a -> s {nextToken = a} :: GetBehaviorModelTrainingSummariesResponse)

-- | A list of all ML Detect behaviors and their model status for a given
-- Security Profile.
getBehaviorModelTrainingSummariesResponse_summaries :: Lens.Lens' GetBehaviorModelTrainingSummariesResponse (Core.Maybe [BehaviorModelTrainingSummary])
getBehaviorModelTrainingSummariesResponse_summaries = Lens.lens (\GetBehaviorModelTrainingSummariesResponse' {summaries} -> summaries) (\s@GetBehaviorModelTrainingSummariesResponse' {} a -> s {summaries = a} :: GetBehaviorModelTrainingSummariesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBehaviorModelTrainingSummariesResponse_httpStatus :: Lens.Lens' GetBehaviorModelTrainingSummariesResponse Core.Int
getBehaviorModelTrainingSummariesResponse_httpStatus = Lens.lens (\GetBehaviorModelTrainingSummariesResponse' {httpStatus} -> httpStatus) (\s@GetBehaviorModelTrainingSummariesResponse' {} a -> s {httpStatus = a} :: GetBehaviorModelTrainingSummariesResponse)

instance
  Core.NFData
    GetBehaviorModelTrainingSummariesResponse
