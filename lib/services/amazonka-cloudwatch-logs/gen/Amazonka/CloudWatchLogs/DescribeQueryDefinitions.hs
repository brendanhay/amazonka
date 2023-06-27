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
-- Module      : Amazonka.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudWatchLogs.DescribeQueryDefinitions
  ( -- * Creating a Request
    DescribeQueryDefinitions (..),
    newDescribeQueryDefinitions,

    -- * Request Lenses
    describeQueryDefinitions_maxResults,
    describeQueryDefinitions_nextToken,
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { -- | Limits the number of returned query definitions to the specified number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to filter your results to only the query definitions
    -- that have names that start with the prefix you specify.
    queryDefinitionNamePrefix :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'describeQueryDefinitions_maxResults' - Limits the number of returned query definitions to the specified number.
--
-- 'nextToken', 'describeQueryDefinitions_nextToken' - Undocumented member.
--
-- 'queryDefinitionNamePrefix', 'describeQueryDefinitions_queryDefinitionNamePrefix' - Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
newDescribeQueryDefinitions ::
  DescribeQueryDefinitions
newDescribeQueryDefinitions =
  DescribeQueryDefinitions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryDefinitionNamePrefix = Prelude.Nothing
    }

-- | Limits the number of returned query definitions to the specified number.
describeQueryDefinitions_maxResults :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Natural)
describeQueryDefinitions_maxResults = Lens.lens (\DescribeQueryDefinitions' {maxResults} -> maxResults) (\s@DescribeQueryDefinitions' {} a -> s {maxResults = a} :: DescribeQueryDefinitions)

-- | Undocumented member.
describeQueryDefinitions_nextToken :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Text)
describeQueryDefinitions_nextToken = Lens.lens (\DescribeQueryDefinitions' {nextToken} -> nextToken) (\s@DescribeQueryDefinitions' {} a -> s {nextToken = a} :: DescribeQueryDefinitions)

-- | Use this parameter to filter your results to only the query definitions
-- that have names that start with the prefix you specify.
describeQueryDefinitions_queryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Prelude.Maybe Prelude.Text)
describeQueryDefinitions_queryDefinitionNamePrefix = Lens.lens (\DescribeQueryDefinitions' {queryDefinitionNamePrefix} -> queryDefinitionNamePrefix) (\s@DescribeQueryDefinitions' {} a -> s {queryDefinitionNamePrefix = a} :: DescribeQueryDefinitions)

instance Core.AWSRequest DescribeQueryDefinitions where
  type
    AWSResponse DescribeQueryDefinitions =
      DescribeQueryDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQueryDefinitionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "queryDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeQueryDefinitions where
  hashWithSalt _salt DescribeQueryDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryDefinitionNamePrefix

instance Prelude.NFData DescribeQueryDefinitions where
  rnf DescribeQueryDefinitions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryDefinitionNamePrefix

instance Data.ToHeaders DescribeQueryDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeQueryDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeQueryDefinitions where
  toJSON DescribeQueryDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("queryDefinitionNamePrefix" Data..=)
              Prelude.<$> queryDefinitionNamePrefix
          ]
      )

instance Data.ToPath DescribeQueryDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeQueryDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of query definitions that match your request.
    queryDefinitions :: Prelude.Maybe [QueryDefinition],
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
-- 'nextToken', 'describeQueryDefinitionsResponse_nextToken' - Undocumented member.
--
-- 'queryDefinitions', 'describeQueryDefinitionsResponse_queryDefinitions' - The list of query definitions that match your request.
--
-- 'httpStatus', 'describeQueryDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeQueryDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQueryDefinitionsResponse
newDescribeQueryDefinitionsResponse pHttpStatus_ =
  DescribeQueryDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      queryDefinitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeQueryDefinitionsResponse_nextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Prelude.Maybe Prelude.Text)
describeQueryDefinitionsResponse_nextToken = Lens.lens (\DescribeQueryDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeQueryDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeQueryDefinitionsResponse)

-- | The list of query definitions that match your request.
describeQueryDefinitionsResponse_queryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Prelude.Maybe [QueryDefinition])
describeQueryDefinitionsResponse_queryDefinitions = Lens.lens (\DescribeQueryDefinitionsResponse' {queryDefinitions} -> queryDefinitions) (\s@DescribeQueryDefinitionsResponse' {} a -> s {queryDefinitions = a} :: DescribeQueryDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeQueryDefinitionsResponse_httpStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Prelude.Int
describeQueryDefinitionsResponse_httpStatus = Lens.lens (\DescribeQueryDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeQueryDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeQueryDefinitionsResponse)

instance
  Prelude.NFData
    DescribeQueryDefinitionsResponse
  where
  rnf DescribeQueryDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryDefinitions
      `Prelude.seq` Prelude.rnf httpStatus
