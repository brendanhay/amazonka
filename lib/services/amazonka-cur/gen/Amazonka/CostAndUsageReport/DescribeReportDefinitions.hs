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
-- Module      : Amazonka.CostAndUsageReport.DescribeReportDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Cost and Usage reports available to this account.
--
-- This operation returns paginated results.
module Amazonka.CostAndUsageReport.DescribeReportDefinitions
  ( -- * Creating a Request
    DescribeReportDefinitions (..),
    newDescribeReportDefinitions,

    -- * Request Lenses
    describeReportDefinitions_maxResults,
    describeReportDefinitions_nextToken,

    -- * Destructuring the Response
    DescribeReportDefinitionsResponse (..),
    newDescribeReportDefinitionsResponse,

    -- * Response Lenses
    describeReportDefinitionsResponse_nextToken,
    describeReportDefinitionsResponse_reportDefinitions,
    describeReportDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostAndUsageReport.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Requests a list of AWS Cost and Usage reports owned by the account.
--
-- /See:/ 'newDescribeReportDefinitions' smart constructor.
data DescribeReportDefinitions = DescribeReportDefinitions'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeReportDefinitions_maxResults' - Undocumented member.
--
-- 'nextToken', 'describeReportDefinitions_nextToken' - Undocumented member.
newDescribeReportDefinitions ::
  DescribeReportDefinitions
newDescribeReportDefinitions =
  DescribeReportDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
describeReportDefinitions_maxResults :: Lens.Lens' DescribeReportDefinitions (Prelude.Maybe Prelude.Natural)
describeReportDefinitions_maxResults = Lens.lens (\DescribeReportDefinitions' {maxResults} -> maxResults) (\s@DescribeReportDefinitions' {} a -> s {maxResults = a} :: DescribeReportDefinitions)

-- | Undocumented member.
describeReportDefinitions_nextToken :: Lens.Lens' DescribeReportDefinitions (Prelude.Maybe Prelude.Text)
describeReportDefinitions_nextToken = Lens.lens (\DescribeReportDefinitions' {nextToken} -> nextToken) (\s@DescribeReportDefinitions' {} a -> s {nextToken = a} :: DescribeReportDefinitions)

instance Core.AWSPager DescribeReportDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReportDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReportDefinitionsResponse_reportDefinitions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeReportDefinitions_nextToken
              Lens..~ rs
              Lens.^? describeReportDefinitionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeReportDefinitions where
  type
    AWSResponse DescribeReportDefinitions =
      DescribeReportDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportDefinitionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ReportDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReportDefinitions where
  hashWithSalt _salt DescribeReportDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeReportDefinitions where
  rnf DescribeReportDefinitions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders DescribeReportDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrigamiServiceGatewayService.DescribeReportDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReportDefinitions where
  toJSON DescribeReportDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeReportDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReportDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200
-- response.
--
-- /See:/ 'newDescribeReportDefinitionsResponse' smart constructor.
data DescribeReportDefinitionsResponse = DescribeReportDefinitionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of AWS Cost and Usage reports owned by the account.
    reportDefinitions :: Prelude.Maybe [ReportDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReportDefinitionsResponse_nextToken' - Undocumented member.
--
-- 'reportDefinitions', 'describeReportDefinitionsResponse_reportDefinitions' - A list of AWS Cost and Usage reports owned by the account.
--
-- 'httpStatus', 'describeReportDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeReportDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReportDefinitionsResponse
newDescribeReportDefinitionsResponse pHttpStatus_ =
  DescribeReportDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      reportDefinitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeReportDefinitionsResponse_nextToken :: Lens.Lens' DescribeReportDefinitionsResponse (Prelude.Maybe Prelude.Text)
describeReportDefinitionsResponse_nextToken = Lens.lens (\DescribeReportDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeReportDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeReportDefinitionsResponse)

-- | A list of AWS Cost and Usage reports owned by the account.
describeReportDefinitionsResponse_reportDefinitions :: Lens.Lens' DescribeReportDefinitionsResponse (Prelude.Maybe [ReportDefinition])
describeReportDefinitionsResponse_reportDefinitions = Lens.lens (\DescribeReportDefinitionsResponse' {reportDefinitions} -> reportDefinitions) (\s@DescribeReportDefinitionsResponse' {} a -> s {reportDefinitions = a} :: DescribeReportDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReportDefinitionsResponse_httpStatus :: Lens.Lens' DescribeReportDefinitionsResponse Prelude.Int
describeReportDefinitionsResponse_httpStatus = Lens.lens (\DescribeReportDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeReportDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeReportDefinitionsResponse)

instance
  Prelude.NFData
    DescribeReportDefinitionsResponse
  where
  rnf DescribeReportDefinitionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf reportDefinitions `Prelude.seq`
        Prelude.rnf httpStatus
