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
    describeQueryDefinitions_nextToken,
    describeQueryDefinitions_maxResults,
    describeQueryDefinitions_queryDefinitionNamePrefix,

    -- * Destructuring the Response
    DescribeQueryDefinitionsResponse (..),
    newDescribeQueryDefinitionsResponse,

    -- * Response Lenses
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { nextToken :: Core.Maybe Core.Text,
    -- | Limits the number of returned query definitions to the specified number.
    maxResults :: Core.Maybe Core.Natural,
    -- | Use this parameter to filter your results to only the query definitions
    -- that have names that start with the prefix you specify.
    queryDefinitionNamePrefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeQueryDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeQueryDefinitions_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeQueryDefinitions_maxResults' - Limits the number of returned query definitions to the specified number.
--
-- 'queryDefinitionNamePrefix', 'describeQueryDefinitions_queryDefinitionNamePrefix' - Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
newDescribeQueryDefinitions ::
  DescribeQueryDefinitions
newDescribeQueryDefinitions =
  DescribeQueryDefinitions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      queryDefinitionNamePrefix = Core.Nothing
    }

-- | Undocumented member.
describeQueryDefinitions_nextToken :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Core.Text)
describeQueryDefinitions_nextToken = Lens.lens (\DescribeQueryDefinitions' {nextToken} -> nextToken) (\s@DescribeQueryDefinitions' {} a -> s {nextToken = a} :: DescribeQueryDefinitions)

-- | Limits the number of returned query definitions to the specified number.
describeQueryDefinitions_maxResults :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Core.Natural)
describeQueryDefinitions_maxResults = Lens.lens (\DescribeQueryDefinitions' {maxResults} -> maxResults) (\s@DescribeQueryDefinitions' {} a -> s {maxResults = a} :: DescribeQueryDefinitions)

-- | Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
describeQueryDefinitions_queryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Core.Text)
describeQueryDefinitions_queryDefinitionNamePrefix = Lens.lens (\DescribeQueryDefinitions' {queryDefinitionNamePrefix} -> queryDefinitionNamePrefix) (\s@DescribeQueryDefinitions' {} a -> s {queryDefinitionNamePrefix = a} :: DescribeQueryDefinitions)

instance Core.AWSRequest DescribeQueryDefinitions where
  type
    AWSResponse DescribeQueryDefinitions =
      DescribeQueryDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "queryDefinitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeQueryDefinitions

instance Core.NFData DescribeQueryDefinitions

instance Core.ToHeaders DescribeQueryDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeQueryDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("queryDefinitionNamePrefix" Core..=)
              Core.<$> queryDefinitionNamePrefix
          ]
      )

instance Core.ToPath DescribeQueryDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeQueryDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The list of query definitions that match your request.
    queryDefinitions :: Core.Maybe [QueryDefinition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeQueryDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeQueryDefinitionsResponse_nextToken' - Undocumented member.
--
-- 'queryDefinitions', 'describeQueryDefinitionsResponse_queryDefinitions' - The list of query definitions that match your request.
--
-- 'httpStatus', 'describeQueryDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeQueryDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeQueryDefinitionsResponse
newDescribeQueryDefinitionsResponse pHttpStatus_ =
  DescribeQueryDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      queryDefinitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeQueryDefinitionsResponse_nextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe Core.Text)
describeQueryDefinitionsResponse_nextToken = Lens.lens (\DescribeQueryDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeQueryDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeQueryDefinitionsResponse)

-- | The list of query definitions that match your request.
describeQueryDefinitionsResponse_queryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe [QueryDefinition])
describeQueryDefinitionsResponse_queryDefinitions = Lens.lens (\DescribeQueryDefinitionsResponse' {queryDefinitions} -> queryDefinitions) (\s@DescribeQueryDefinitionsResponse' {} a -> s {queryDefinitions = a} :: DescribeQueryDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeQueryDefinitionsResponse_httpStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Core.Int
describeQueryDefinitionsResponse_httpStatus = Lens.lens (\DescribeQueryDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeQueryDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeQueryDefinitionsResponse)

instance Core.NFData DescribeQueryDefinitionsResponse
