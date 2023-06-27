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
-- Module      : Amazonka.Synthetics.DescribeRuntimeVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Synthetics canary runtime versions. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
module Amazonka.Synthetics.DescribeRuntimeVersions
  ( -- * Creating a Request
    DescribeRuntimeVersions (..),
    newDescribeRuntimeVersions,

    -- * Request Lenses
    describeRuntimeVersions_maxResults,
    describeRuntimeVersions_nextToken,

    -- * Destructuring the Response
    DescribeRuntimeVersionsResponse (..),
    newDescribeRuntimeVersionsResponse,

    -- * Response Lenses
    describeRuntimeVersionsResponse_nextToken,
    describeRuntimeVersionsResponse_runtimeVersions,
    describeRuntimeVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDescribeRuntimeVersions' smart constructor.
data DescribeRuntimeVersions = DescribeRuntimeVersions'
  { -- | Specify this parameter to limit how many runs are returned each time you
    -- use the @DescribeRuntimeVersions@ operation. If you omit this parameter,
    -- the default of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeRuntimeVersions@ operation to
    -- retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuntimeVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeRuntimeVersions_maxResults' - Specify this parameter to limit how many runs are returned each time you
-- use the @DescribeRuntimeVersions@ operation. If you omit this parameter,
-- the default of 100 is used.
--
-- 'nextToken', 'describeRuntimeVersions_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeRuntimeVersions@ operation to
-- retrieve the next set of results.
newDescribeRuntimeVersions ::
  DescribeRuntimeVersions
newDescribeRuntimeVersions =
  DescribeRuntimeVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specify this parameter to limit how many runs are returned each time you
-- use the @DescribeRuntimeVersions@ operation. If you omit this parameter,
-- the default of 100 is used.
describeRuntimeVersions_maxResults :: Lens.Lens' DescribeRuntimeVersions (Prelude.Maybe Prelude.Natural)
describeRuntimeVersions_maxResults = Lens.lens (\DescribeRuntimeVersions' {maxResults} -> maxResults) (\s@DescribeRuntimeVersions' {} a -> s {maxResults = a} :: DescribeRuntimeVersions)

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeRuntimeVersions@ operation to
-- retrieve the next set of results.
describeRuntimeVersions_nextToken :: Lens.Lens' DescribeRuntimeVersions (Prelude.Maybe Prelude.Text)
describeRuntimeVersions_nextToken = Lens.lens (\DescribeRuntimeVersions' {nextToken} -> nextToken) (\s@DescribeRuntimeVersions' {} a -> s {nextToken = a} :: DescribeRuntimeVersions)

instance Core.AWSRequest DescribeRuntimeVersions where
  type
    AWSResponse DescribeRuntimeVersions =
      DescribeRuntimeVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuntimeVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RuntimeVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRuntimeVersions where
  hashWithSalt _salt DescribeRuntimeVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeRuntimeVersions where
  rnf DescribeRuntimeVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeRuntimeVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRuntimeVersions where
  toJSON DescribeRuntimeVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeRuntimeVersions where
  toPath = Prelude.const "/runtime-versions"

instance Data.ToQuery DescribeRuntimeVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRuntimeVersionsResponse' smart constructor.
data DescribeRuntimeVersionsResponse = DescribeRuntimeVersionsResponse'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeRuntimeVersions@ operation to
    -- retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that display the details about each Synthetics
    -- canary runtime version.
    runtimeVersions :: Prelude.Maybe [RuntimeVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuntimeVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRuntimeVersionsResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeRuntimeVersions@ operation to
-- retrieve the next set of results.
--
-- 'runtimeVersions', 'describeRuntimeVersionsResponse_runtimeVersions' - An array of objects that display the details about each Synthetics
-- canary runtime version.
--
-- 'httpStatus', 'describeRuntimeVersionsResponse_httpStatus' - The response's http status code.
newDescribeRuntimeVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRuntimeVersionsResponse
newDescribeRuntimeVersionsResponse pHttpStatus_ =
  DescribeRuntimeVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      runtimeVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeRuntimeVersions@ operation to
-- retrieve the next set of results.
describeRuntimeVersionsResponse_nextToken :: Lens.Lens' DescribeRuntimeVersionsResponse (Prelude.Maybe Prelude.Text)
describeRuntimeVersionsResponse_nextToken = Lens.lens (\DescribeRuntimeVersionsResponse' {nextToken} -> nextToken) (\s@DescribeRuntimeVersionsResponse' {} a -> s {nextToken = a} :: DescribeRuntimeVersionsResponse)

-- | An array of objects that display the details about each Synthetics
-- canary runtime version.
describeRuntimeVersionsResponse_runtimeVersions :: Lens.Lens' DescribeRuntimeVersionsResponse (Prelude.Maybe [RuntimeVersion])
describeRuntimeVersionsResponse_runtimeVersions = Lens.lens (\DescribeRuntimeVersionsResponse' {runtimeVersions} -> runtimeVersions) (\s@DescribeRuntimeVersionsResponse' {} a -> s {runtimeVersions = a} :: DescribeRuntimeVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRuntimeVersionsResponse_httpStatus :: Lens.Lens' DescribeRuntimeVersionsResponse Prelude.Int
describeRuntimeVersionsResponse_httpStatus = Lens.lens (\DescribeRuntimeVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeRuntimeVersionsResponse' {} a -> s {httpStatus = a} :: DescribeRuntimeVersionsResponse)

instance
  Prelude.NFData
    DescribeRuntimeVersionsResponse
  where
  rnf DescribeRuntimeVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf runtimeVersions
      `Prelude.seq` Prelude.rnf httpStatus
