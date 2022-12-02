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
-- Module      : Amazonka.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Config.DescribeConformancePackStatus
  ( -- * Creating a Request
    DescribeConformancePackStatus (..),
    newDescribeConformancePackStatus,

    -- * Request Lenses
    describeConformancePackStatus_nextToken,
    describeConformancePackStatus_limit,
    describeConformancePackStatus_conformancePackNames,

    -- * Destructuring the Response
    DescribeConformancePackStatusResponse (..),
    newDescribeConformancePackStatusResponse,

    -- * Response Lenses
    describeConformancePackStatusResponse_conformancePackStatusDetails,
    describeConformancePackStatusResponse_nextToken,
    describeConformancePackStatusResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conformance packs status returned on each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Comma-separated list of conformance pack names.
    conformancePackNames :: Prelude.Maybe [Prelude.Text]
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
-- 'nextToken', 'describeConformancePackStatus_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'limit', 'describeConformancePackStatus_limit' - The maximum number of conformance packs status returned on each page.
--
-- 'conformancePackNames', 'describeConformancePackStatus_conformancePackNames' - Comma-separated list of conformance pack names.
newDescribeConformancePackStatus ::
  DescribeConformancePackStatus
newDescribeConformancePackStatus =
  DescribeConformancePackStatus'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      conformancePackNames = Prelude.Nothing
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeConformancePackStatus_nextToken :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe Prelude.Text)
describeConformancePackStatus_nextToken = Lens.lens (\DescribeConformancePackStatus' {nextToken} -> nextToken) (\s@DescribeConformancePackStatus' {} a -> s {nextToken = a} :: DescribeConformancePackStatus)

-- | The maximum number of conformance packs status returned on each page.
describeConformancePackStatus_limit :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe Prelude.Natural)
describeConformancePackStatus_limit = Lens.lens (\DescribeConformancePackStatus' {limit} -> limit) (\s@DescribeConformancePackStatus' {} a -> s {limit = a} :: DescribeConformancePackStatus)

-- | Comma-separated list of conformance pack names.
describeConformancePackStatus_conformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Prelude.Maybe [Prelude.Text])
describeConformancePackStatus_conformancePackNames = Lens.lens (\DescribeConformancePackStatus' {conformancePackNames} -> conformancePackNames) (\s@DescribeConformancePackStatus' {} a -> s {conformancePackNames = a} :: DescribeConformancePackStatus) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConformancePackStatusResponse'
            Prelude.<$> ( x Data..?> "ConformancePackStatusDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConformancePackStatus
  where
  hashWithSalt _salt DescribeConformancePackStatus' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` conformancePackNames

instance Prelude.NFData DescribeConformancePackStatus where
  rnf DescribeConformancePackStatus' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf conformancePackNames

instance Data.ToHeaders DescribeConformancePackStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConformancePackStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Limit" Data..=) Prelude.<$> limit,
            ("ConformancePackNames" Data..=)
              Prelude.<$> conformancePackNames
          ]
      )

instance Data.ToPath DescribeConformancePackStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConformancePackStatus where
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
  where
  rnf DescribeConformancePackStatusResponse' {..} =
    Prelude.rnf conformancePackStatusDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
