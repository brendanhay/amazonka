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
-- Module      : Network.AWS.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides one or more conformance packs deployment status.
--
-- If there are no conformance packs then you will see an empty result.
module Network.AWS.Config.DescribeConformancePackStatus
  ( -- * Creating a Request
    DescribeConformancePackStatus (..),
    newDescribeConformancePackStatus,

    -- * Request Lenses
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatus_limit,

    -- * Destructuring the Response
    DescribeConformancePackStatusResponse (..),
    newDescribeConformancePackStatusResponse,

    -- * Response Lenses
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Comma-separated list of conformance pack names.
    conformancePackNames :: Core.Maybe [Core.Text],
    -- | The maximum number of conformance packs status returned on each page.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConformancePackStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConformancePackStatus_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackNames', 'describeConformancePackStatus_conformancePackNames' - Comma-separated list of conformance pack names.
--
-- 'limit', 'describeConformancePackStatus_limit' - The maximum number of conformance packs status returned on each page.
newDescribeConformancePackStatus ::
  DescribeConformancePackStatus
newDescribeConformancePackStatus =
  DescribeConformancePackStatus'
    { nextToken =
        Core.Nothing,
      conformancePackNames = Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackStatus_nextToken :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Core.Text)
describeConformancePackStatus_nextToken = Lens.lens (\DescribeConformancePackStatus' {nextToken} -> nextToken) (\s@DescribeConformancePackStatus' {} a -> s {nextToken = a} :: DescribeConformancePackStatus)

-- | Comma-separated list of conformance pack names.
describeConformancePackStatus_conformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe [Core.Text])
describeConformancePackStatus_conformancePackNames = Lens.lens (\DescribeConformancePackStatus' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePackStatus' {} a -> s {conformancePackNames = a} :: DescribeConformancePackStatus) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of conformance packs status returned on each page.
describeConformancePackStatus_limit :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Core.Natural)
describeConformancePackStatus_limit = Lens.lens (\DescribeConformancePackStatus' {limit} -> limit) (\s@DescribeConformancePackStatus' {} a -> s {limit = a} :: DescribeConformancePackStatus)

instance
  Core.AWSRequest
    DescribeConformancePackStatus
  where
  type
    AWSResponse DescribeConformancePackStatus =
      DescribeConformancePackStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePackStatusResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ConformancePackStatusDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConformancePackStatus

instance Core.NFData DescribeConformancePackStatus

instance Core.ToHeaders DescribeConformancePackStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConformancePackStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConformancePackNames" Core..=)
              Core.<$> conformancePackNames,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeConformancePackStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConformancePackStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @ConformancePackStatusDetail@ objects.
    conformancePackStatusDetails :: Core.Maybe [ConformancePackStatusDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConformancePackStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConformancePackStatusResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'conformancePackStatusDetails', 'describeConformancePackStatusResponse_conformancePackStatusDetails' - A list of @ConformancePackStatusDetail@ objects.
--
-- 'httpStatus', 'describeConformancePackStatusResponse_httpStatus' - The response's http status code.
newDescribeConformancePackStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConformancePackStatusResponse
newDescribeConformancePackStatusResponse pHttpStatus_ =
  DescribeConformancePackStatusResponse'
    { nextToken =
        Core.Nothing,
      conformancePackStatusDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackStatusResponse_nextToken :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe Core.Text)
describeConformancePackStatusResponse_nextToken = Lens.lens (\DescribeConformancePackStatusResponse' {nextToken} -> nextToken) (\s@DescribeConformancePackStatusResponse' {} a -> s {nextToken = a} :: DescribeConformancePackStatusResponse)

-- | A list of @ConformancePackStatusDetail@ objects.
describeConformancePackStatusResponse_conformancePackStatusDetails :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe [ConformancePackStatusDetail])
describeConformancePackStatusResponse_conformancePackStatusDetails = Lens.lens (\DescribeConformancePackStatusResponse' {conformancePackStatusDetails} -> conformancePackStatusDetails) (\s@DescribeConformancePackStatusResponse' {} a -> s {conformancePackStatusDetails = a} :: DescribeConformancePackStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConformancePackStatusResponse_httpStatus :: Lens.Lens' DescribeConformancePackStatusResponse Core.Int
describeConformancePackStatusResponse_httpStatus = Lens.lens (\DescribeConformancePackStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePackStatusResponse' {} a -> s {httpStatus = a} :: DescribeConformancePackStatusResponse)

instance
  Core.NFData
    DescribeConformancePackStatusResponse
