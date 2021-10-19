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
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a paginated list of your saved CloudWatch Logs
-- Insights query definitions.
--
-- You can use the @queryDefinitionNamePrefix@ parameter to limit the
-- results to only the query definitions that have names that start with a
-- certain string.
module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
  ( -- * Creating a Request
    DescribeQueryDefinitions (..),
    newDescribeQueryDefinitions,

    -- * Request Lenses
    describeQueryDefinitions_queryDefinitionNamePrefix,
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_maxResults,

    -- * Destructuring the Response
    DescribeQueryDefinitionsResponse (..),
    newDescribeQueryDefinitionsResponse,

    -- * Response Lenses
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { -- | Use this parameter to filter your results to only the query definitions
    -- that have names that start with the prefix you specify.
    queryDefinitionNamePrefix :: Prelude.Maybe Prelude.Text,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limits the number of returned query definitions to the specified number.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQueryDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryDefinitionNamePrefix', 'describeQueryDefinitions_queryDefinitionNamePrefix' - Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
--
-- 'nextToken', 'describeQueryDefinitions_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeQueryDefinitions_maxResults' - Limits the number of returned query definitions to the specified number.
newDescribeQueryDefinitions ::
  DescribeQueryDefinitions
newDescribeQueryDefinitions =
  DescribeQueryDefinitions'
    { queryDefinitionNamePrefix =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
describeQueryDefinitions_queryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Text)
describeQueryDefinitions_queryDefinitionNamePrefix = Lens.lens (\DescribeQueryDefinitions' {queryDefinitionNamePrefix} -> queryDefinitionNamePrefix) (\s@DescribeQueryDefinitions' {} a -> s {queryDefinitionNamePrefix = a} :: DescribeQueryDefinitions)

-- | Undocumented member.
describeQueryDefinitions_nextToken :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Text)
describeQueryDefinitions_nextToken = Lens.lens (\DescribeQueryDefinitions' {nextToken} -> nextToken) (\s@DescribeQueryDefinitions' {} a -> s {nextToken = a} :: DescribeQueryDefinitions)

-- | Limits the number of returned query definitions to the specified number.
describeQueryDefinitions_maxResults :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Natural)
describeQueryDefinitions_maxResults = Lens.lens (\DescribeQueryDefinitions' {maxResults} -> maxResults) (\s@DescribeQueryDefinitions' {} a -> s {maxResults = a} :: DescribeQueryDefinitions)

instance Core.AWSRequest DescribeQueryDefinitions where
  type
    AWSResponse DescribeQueryDefinitions =
      DescribeQueryDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            Prelude.<$> ( x Core..?> "queryDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeQueryDefinitions

instance Prelude.NFData DescribeQueryDefinitions

instance Core.ToHeaders DescribeQueryDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeQueryDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("queryDefinitionNamePrefix" Core..=)
              Prelude.<$> queryDefinitionNamePrefix,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeQueryDefinitions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeQueryDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { -- | The list of query definitions that match your request.
    queryDefinitions :: Prelude.Maybe [QueryDefinition],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQueryDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryDefinitions', 'describeQueryDefinitionsResponse_queryDefinitions' - The list of query definitions that match your request.
--
-- 'nextToken', 'describeQueryDefinitionsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeQueryDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeQueryDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQueryDefinitionsResponse
newDescribeQueryDefinitionsResponse pHttpStatus_ =
  DescribeQueryDefinitionsResponse'
    { queryDefinitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of query definitions that match your request.
describeQueryDefinitionsResponse_queryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Prelude.Maybe [QueryDefinition])
describeQueryDefinitionsResponse_queryDefinitions = Lens.lens (\DescribeQueryDefinitionsResponse' {queryDefinitions} -> queryDefinitions) (\s@DescribeQueryDefinitionsResponse' {} a -> s {queryDefinitions = a} :: DescribeQueryDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeQueryDefinitionsResponse_nextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Prelude.Maybe Prelude.Text)
describeQueryDefinitionsResponse_nextToken = Lens.lens (\DescribeQueryDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeQueryDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeQueryDefinitionsResponse)

-- | The response's http status code.
describeQueryDefinitionsResponse_httpStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Prelude.Int
describeQueryDefinitionsResponse_httpStatus = Lens.lens (\DescribeQueryDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeQueryDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeQueryDefinitionsResponse)

instance
  Prelude.NFData
    DescribeQueryDefinitionsResponse
