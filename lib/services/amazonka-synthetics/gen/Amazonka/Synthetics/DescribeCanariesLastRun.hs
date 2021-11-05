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
-- Module      : Amazonka.Synthetics.DescribeCanariesLastRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to see information from the most recent run of each
-- canary that you have created.
module Amazonka.Synthetics.DescribeCanariesLastRun
  ( -- * Creating a Request
    DescribeCanariesLastRun (..),
    newDescribeCanariesLastRun,

    -- * Request Lenses
    describeCanariesLastRun_nextToken,
    describeCanariesLastRun_maxResults,

    -- * Destructuring the Response
    DescribeCanariesLastRunResponse (..),
    newDescribeCanariesLastRunResponse,

    -- * Response Lenses
    describeCanariesLastRunResponse_nextToken,
    describeCanariesLastRunResponse_canariesLastRun,
    describeCanariesLastRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDescribeCanariesLastRun' smart constructor.
data DescribeCanariesLastRun = DescribeCanariesLastRun'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeCanaries@ operation to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify this parameter to limit how many runs are returned each time you
    -- use the @DescribeLastRun@ operation. If you omit this parameter, the
    -- default of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanariesLastRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCanariesLastRun_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
--
-- 'maxResults', 'describeCanariesLastRun_maxResults' - Specify this parameter to limit how many runs are returned each time you
-- use the @DescribeLastRun@ operation. If you omit this parameter, the
-- default of 100 is used.
newDescribeCanariesLastRun ::
  DescribeCanariesLastRun
newDescribeCanariesLastRun =
  DescribeCanariesLastRun'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
describeCanariesLastRun_nextToken :: Lens.Lens' DescribeCanariesLastRun (Prelude.Maybe Prelude.Text)
describeCanariesLastRun_nextToken = Lens.lens (\DescribeCanariesLastRun' {nextToken} -> nextToken) (\s@DescribeCanariesLastRun' {} a -> s {nextToken = a} :: DescribeCanariesLastRun)

-- | Specify this parameter to limit how many runs are returned each time you
-- use the @DescribeLastRun@ operation. If you omit this parameter, the
-- default of 100 is used.
describeCanariesLastRun_maxResults :: Lens.Lens' DescribeCanariesLastRun (Prelude.Maybe Prelude.Natural)
describeCanariesLastRun_maxResults = Lens.lens (\DescribeCanariesLastRun' {maxResults} -> maxResults) (\s@DescribeCanariesLastRun' {} a -> s {maxResults = a} :: DescribeCanariesLastRun)

instance Core.AWSRequest DescribeCanariesLastRun where
  type
    AWSResponse DescribeCanariesLastRun =
      DescribeCanariesLastRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCanariesLastRunResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CanariesLastRun"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCanariesLastRun

instance Prelude.NFData DescribeCanariesLastRun

instance Core.ToHeaders DescribeCanariesLastRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCanariesLastRun where
  toJSON DescribeCanariesLastRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeCanariesLastRun where
  toPath = Prelude.const "/canaries/last-run"

instance Core.ToQuery DescribeCanariesLastRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCanariesLastRunResponse' smart constructor.
data DescribeCanariesLastRunResponse = DescribeCanariesLastRunResponse'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeCanariesLastRun@ operation to
    -- retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that contains the information from the most recent run of each
    -- canary.
    canariesLastRun :: Prelude.Maybe [CanaryLastRun],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanariesLastRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCanariesLastRunResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanariesLastRun@ operation to
-- retrieve the next set of results.
--
-- 'canariesLastRun', 'describeCanariesLastRunResponse_canariesLastRun' - An array that contains the information from the most recent run of each
-- canary.
--
-- 'httpStatus', 'describeCanariesLastRunResponse_httpStatus' - The response's http status code.
newDescribeCanariesLastRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCanariesLastRunResponse
newDescribeCanariesLastRunResponse pHttpStatus_ =
  DescribeCanariesLastRunResponse'
    { nextToken =
        Prelude.Nothing,
      canariesLastRun = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanariesLastRun@ operation to
-- retrieve the next set of results.
describeCanariesLastRunResponse_nextToken :: Lens.Lens' DescribeCanariesLastRunResponse (Prelude.Maybe Prelude.Text)
describeCanariesLastRunResponse_nextToken = Lens.lens (\DescribeCanariesLastRunResponse' {nextToken} -> nextToken) (\s@DescribeCanariesLastRunResponse' {} a -> s {nextToken = a} :: DescribeCanariesLastRunResponse)

-- | An array that contains the information from the most recent run of each
-- canary.
describeCanariesLastRunResponse_canariesLastRun :: Lens.Lens' DescribeCanariesLastRunResponse (Prelude.Maybe [CanaryLastRun])
describeCanariesLastRunResponse_canariesLastRun = Lens.lens (\DescribeCanariesLastRunResponse' {canariesLastRun} -> canariesLastRun) (\s@DescribeCanariesLastRunResponse' {} a -> s {canariesLastRun = a} :: DescribeCanariesLastRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCanariesLastRunResponse_httpStatus :: Lens.Lens' DescribeCanariesLastRunResponse Prelude.Int
describeCanariesLastRunResponse_httpStatus = Lens.lens (\DescribeCanariesLastRunResponse' {httpStatus} -> httpStatus) (\s@DescribeCanariesLastRunResponse' {} a -> s {httpStatus = a} :: DescribeCanariesLastRunResponse)

instance
  Prelude.NFData
    DescribeCanariesLastRunResponse
