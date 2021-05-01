{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostAndUsageReport.DescribeReportDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Cost and Usage reports available to this account.
--
-- This operation returns paginated results.
module Network.AWS.CostAndUsageReport.DescribeReportDefinitions
  ( -- * Creating a Request
    DescribeReportDefinitions (..),
    newDescribeReportDefinitions,

    -- * Request Lenses
    describeReportDefinitions_nextToken,
    describeReportDefinitions_maxResults,

    -- * Destructuring the Response
    DescribeReportDefinitionsResponse (..),
    newDescribeReportDefinitionsResponse,

    -- * Response Lenses
    describeReportDefinitionsResponse_nextToken,
    describeReportDefinitionsResponse_reportDefinitions,
    describeReportDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests a list of AWS Cost and Usage reports owned by the account.
--
-- /See:/ 'newDescribeReportDefinitions' smart constructor.
data DescribeReportDefinitions = DescribeReportDefinitions'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReportDefinitions_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeReportDefinitions_maxResults' - Undocumented member.
newDescribeReportDefinitions ::
  DescribeReportDefinitions
newDescribeReportDefinitions =
  DescribeReportDefinitions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
describeReportDefinitions_nextToken :: Lens.Lens' DescribeReportDefinitions (Prelude.Maybe Prelude.Text)
describeReportDefinitions_nextToken = Lens.lens (\DescribeReportDefinitions' {nextToken} -> nextToken) (\s@DescribeReportDefinitions' {} a -> s {nextToken = a} :: DescribeReportDefinitions)

-- | Undocumented member.
describeReportDefinitions_maxResults :: Lens.Lens' DescribeReportDefinitions (Prelude.Maybe Prelude.Natural)
describeReportDefinitions_maxResults = Lens.lens (\DescribeReportDefinitions' {maxResults} -> maxResults) (\s@DescribeReportDefinitions' {} a -> s {maxResults = a} :: DescribeReportDefinitions)

instance Pager.AWSPager DescribeReportDefinitions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeReportDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeReportDefinitionsResponse_reportDefinitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeReportDefinitions_nextToken
          Lens..~ rs
          Lens.^? describeReportDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeReportDefinitions where
  type
    Rs DescribeReportDefinitions =
      DescribeReportDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportDefinitionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ReportDefinitions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReportDefinitions

instance Prelude.NFData DescribeReportDefinitions

instance Prelude.ToHeaders DescribeReportDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrigamiServiceGatewayService.DescribeReportDefinitions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeReportDefinitions where
  toJSON DescribeReportDefinitions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath DescribeReportDefinitions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeReportDefinitions where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeReportDefinitionsResponse_reportDefinitions = Lens.lens (\DescribeReportDefinitionsResponse' {reportDefinitions} -> reportDefinitions) (\s@DescribeReportDefinitionsResponse' {} a -> s {reportDefinitions = a} :: DescribeReportDefinitionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeReportDefinitionsResponse_httpStatus :: Lens.Lens' DescribeReportDefinitionsResponse Prelude.Int
describeReportDefinitionsResponse_httpStatus = Lens.lens (\DescribeReportDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeReportDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeReportDefinitionsResponse)

instance
  Prelude.NFData
    DescribeReportDefinitionsResponse
