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
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConformancePackStatus
  ( -- * Creating a Request
    DescribeConformancePackStatus (..),
    newDescribeConformancePackStatus,

    -- * Request Lenses
    describeConformancePackStatus_conformancePackNames,
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_limit,

    -- * Destructuring the Response
    DescribeConformancePackStatusResponse (..),
    newDescribeConformancePackStatusResponse,

    -- * Response Lenses
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { -- | Comma-separated list of conformance pack names.
    conformancePackNames :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conformance packs status returned on each page.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePackStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackNames', 'describeConformancePackStatus_conformancePackNames' - Comma-separated list of conformance pack names.
--
-- 'nextToken', 'describeConformancePackStatus_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'limit', 'describeConformancePackStatus_limit' - The maximum number of conformance packs status returned on each page.
newDescribeConformancePackStatus ::
  DescribeConformancePackStatus
newDescribeConformancePackStatus =
  DescribeConformancePackStatus'
    { conformancePackNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | Comma-separated list of conformance pack names.
describeConformancePackStatus_conformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe [Prelude.Text])
describeConformancePackStatus_conformancePackNames = Lens.lens (\DescribeConformancePackStatus' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePackStatus' {} a -> s {conformancePackNames = a} :: DescribeConformancePackStatus) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackStatus_nextToken :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe Prelude.Text)
describeConformancePackStatus_nextToken = Lens.lens (\DescribeConformancePackStatus' {nextToken} -> nextToken) (\s@DescribeConformancePackStatus' {} a -> s {nextToken = a} :: DescribeConformancePackStatus)

-- | The maximum number of conformance packs status returned on each page.
describeConformancePackStatus_limit :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe Prelude.Natural)
describeConformancePackStatus_limit = Lens.lens (\DescribeConformancePackStatus' {limit} -> limit) (\s@DescribeConformancePackStatus' {} a -> s {limit = a} :: DescribeConformancePackStatus)

instance Core.AWSPager DescribeConformancePackStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConformancePackStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConformancePackStatusResponse_conformancePackStatusDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeConformancePackStatus_nextToken
          Lens..~ rs
          Lens.^? describeConformancePackStatusResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..?> "ConformancePackStatusDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConformancePackStatus

instance Prelude.NFData DescribeConformancePackStatus

instance Core.ToHeaders DescribeConformancePackStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConformancePackStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConformancePackNames" Core..=)
              Prelude.<$> conformancePackNames,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeConformancePackStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConformancePackStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { -- | A list of @ConformancePackStatusDetail@ objects.
    conformancePackStatusDetails :: Prelude.Maybe [ConformancePackStatusDetail],
    -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConformancePackStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackStatusDetails', 'describeConformancePackStatusResponse_conformancePackStatusDetails' - A list of @ConformancePackStatusDetail@ objects.
--
-- 'nextToken', 'describeConformancePackStatusResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'httpStatus', 'describeConformancePackStatusResponse_httpStatus' - The response's http status code.
newDescribeConformancePackStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConformancePackStatusResponse
newDescribeConformancePackStatusResponse pHttpStatus_ =
  DescribeConformancePackStatusResponse'
    { conformancePackStatusDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ConformancePackStatusDetail@ objects.
describeConformancePackStatusResponse_conformancePackStatusDetails :: Lens.Lens' DescribeConformancePackStatusResponse (Prelude.Maybe [ConformancePackStatusDetail])
describeConformancePackStatusResponse_conformancePackStatusDetails = Lens.lens (\DescribeConformancePackStatusResponse' {conformancePackStatusDetails} -> conformancePackStatusDetails) (\s@DescribeConformancePackStatusResponse' {} a -> s {conformancePackStatusDetails = a} :: DescribeConformancePackStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackStatusResponse_nextToken :: Lens.Lens' DescribeConformancePackStatusResponse (Prelude.Maybe Prelude.Text)
describeConformancePackStatusResponse_nextToken = Lens.lens (\DescribeConformancePackStatusResponse' {nextToken} -> nextToken) (\s@DescribeConformancePackStatusResponse' {} a -> s {nextToken = a} :: DescribeConformancePackStatusResponse)

-- | The response's http status code.
describeConformancePackStatusResponse_httpStatus :: Lens.Lens' DescribeConformancePackStatusResponse Prelude.Int
describeConformancePackStatusResponse_httpStatus = Lens.lens (\DescribeConformancePackStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConformancePackStatusResponse' {} a -> s {httpStatus = a} :: DescribeConformancePackStatusResponse)

instance
  Prelude.NFData
    DescribeConformancePackStatusResponse
