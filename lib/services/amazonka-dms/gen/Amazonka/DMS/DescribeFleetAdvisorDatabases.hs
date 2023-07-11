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
-- Module      : Amazonka.DMS.DescribeFleetAdvisorDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Fleet Advisor databases in your account.
module Amazonka.DMS.DescribeFleetAdvisorDatabases
  ( -- * Creating a Request
    DescribeFleetAdvisorDatabases (..),
    newDescribeFleetAdvisorDatabases,

    -- * Request Lenses
    describeFleetAdvisorDatabases_filters,
    describeFleetAdvisorDatabases_maxRecords,
    describeFleetAdvisorDatabases_nextToken,

    -- * Destructuring the Response
    DescribeFleetAdvisorDatabasesResponse (..),
    newDescribeFleetAdvisorDatabasesResponse,

    -- * Response Lenses
    describeFleetAdvisorDatabasesResponse_databases,
    describeFleetAdvisorDatabasesResponse_nextToken,
    describeFleetAdvisorDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetAdvisorDatabases' smart constructor.
data DescribeFleetAdvisorDatabases = DescribeFleetAdvisorDatabases'
  { -- | If you specify any of the following filters, the output includes
    -- information for only those databases that meet the filter criteria:
    --
    -- -   @database-id@ – The ID of the database.
    --
    -- -   @database-name@ – The name of the database.
    --
    -- -   @database-engine@ – The name of the database engine.
    --
    -- -   @server-ip-address@ – The IP address of the database server.
    --
    -- -   @database-ip-address@ – The IP address of the database.
    --
    -- -   @collector-name@ – The name of the associated Fleet Advisor
    --     collector.
    --
    -- An example is:
    -- @describe-fleet-advisor-databases --filter Name=\"database-id\",Values=\"45\"@
    filters :: Prelude.Maybe [Filter],
    -- | Sets the maximum number of records returned in the response.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | If @NextToken@ is returned by a previous response, there are more
    -- results available. The value of @NextToken@ is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAdvisorDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeFleetAdvisorDatabases_filters' - If you specify any of the following filters, the output includes
-- information for only those databases that meet the filter criteria:
--
-- -   @database-id@ – The ID of the database.
--
-- -   @database-name@ – The name of the database.
--
-- -   @database-engine@ – The name of the database engine.
--
-- -   @server-ip-address@ – The IP address of the database server.
--
-- -   @database-ip-address@ – The IP address of the database.
--
-- -   @collector-name@ – The name of the associated Fleet Advisor
--     collector.
--
-- An example is:
-- @describe-fleet-advisor-databases --filter Name=\"database-id\",Values=\"45\"@
--
-- 'maxRecords', 'describeFleetAdvisorDatabases_maxRecords' - Sets the maximum number of records returned in the response.
--
-- 'nextToken', 'describeFleetAdvisorDatabases_nextToken' - If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
newDescribeFleetAdvisorDatabases ::
  DescribeFleetAdvisorDatabases
newDescribeFleetAdvisorDatabases =
  DescribeFleetAdvisorDatabases'
    { filters =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | If you specify any of the following filters, the output includes
-- information for only those databases that meet the filter criteria:
--
-- -   @database-id@ – The ID of the database.
--
-- -   @database-name@ – The name of the database.
--
-- -   @database-engine@ – The name of the database engine.
--
-- -   @server-ip-address@ – The IP address of the database server.
--
-- -   @database-ip-address@ – The IP address of the database.
--
-- -   @collector-name@ – The name of the associated Fleet Advisor
--     collector.
--
-- An example is:
-- @describe-fleet-advisor-databases --filter Name=\"database-id\",Values=\"45\"@
describeFleetAdvisorDatabases_filters :: Lens.Lens' DescribeFleetAdvisorDatabases (Prelude.Maybe [Filter])
describeFleetAdvisorDatabases_filters = Lens.lens (\DescribeFleetAdvisorDatabases' {filters} -> filters) (\s@DescribeFleetAdvisorDatabases' {} a -> s {filters = a} :: DescribeFleetAdvisorDatabases) Prelude.. Lens.mapping Lens.coerced

-- | Sets the maximum number of records returned in the response.
describeFleetAdvisorDatabases_maxRecords :: Lens.Lens' DescribeFleetAdvisorDatabases (Prelude.Maybe Prelude.Int)
describeFleetAdvisorDatabases_maxRecords = Lens.lens (\DescribeFleetAdvisorDatabases' {maxRecords} -> maxRecords) (\s@DescribeFleetAdvisorDatabases' {} a -> s {maxRecords = a} :: DescribeFleetAdvisorDatabases)

-- | If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeFleetAdvisorDatabases_nextToken :: Lens.Lens' DescribeFleetAdvisorDatabases (Prelude.Maybe Prelude.Text)
describeFleetAdvisorDatabases_nextToken = Lens.lens (\DescribeFleetAdvisorDatabases' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorDatabases' {} a -> s {nextToken = a} :: DescribeFleetAdvisorDatabases)

instance
  Core.AWSRequest
    DescribeFleetAdvisorDatabases
  where
  type
    AWSResponse DescribeFleetAdvisorDatabases =
      DescribeFleetAdvisorDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAdvisorDatabasesResponse'
            Prelude.<$> (x Data..?> "Databases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFleetAdvisorDatabases
  where
  hashWithSalt _salt DescribeFleetAdvisorDatabases' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFleetAdvisorDatabases where
  rnf DescribeFleetAdvisorDatabases' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFleetAdvisorDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeFleetAdvisorDatabases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetAdvisorDatabases where
  toJSON DescribeFleetAdvisorDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFleetAdvisorDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetAdvisorDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetAdvisorDatabasesResponse' smart constructor.
data DescribeFleetAdvisorDatabasesResponse = DescribeFleetAdvisorDatabasesResponse'
  { -- | Provides descriptions of the Fleet Advisor collector databases,
    -- including the database\'s collector, ID, and name.
    databases :: Prelude.Maybe [DatabaseResponse],
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAdvisorDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databases', 'describeFleetAdvisorDatabasesResponse_databases' - Provides descriptions of the Fleet Advisor collector databases,
-- including the database\'s collector, ID, and name.
--
-- 'nextToken', 'describeFleetAdvisorDatabasesResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'httpStatus', 'describeFleetAdvisorDatabasesResponse_httpStatus' - The response's http status code.
newDescribeFleetAdvisorDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetAdvisorDatabasesResponse
newDescribeFleetAdvisorDatabasesResponse pHttpStatus_ =
  DescribeFleetAdvisorDatabasesResponse'
    { databases =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides descriptions of the Fleet Advisor collector databases,
-- including the database\'s collector, ID, and name.
describeFleetAdvisorDatabasesResponse_databases :: Lens.Lens' DescribeFleetAdvisorDatabasesResponse (Prelude.Maybe [DatabaseResponse])
describeFleetAdvisorDatabasesResponse_databases = Lens.lens (\DescribeFleetAdvisorDatabasesResponse' {databases} -> databases) (\s@DescribeFleetAdvisorDatabasesResponse' {} a -> s {databases = a} :: DescribeFleetAdvisorDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
describeFleetAdvisorDatabasesResponse_nextToken :: Lens.Lens' DescribeFleetAdvisorDatabasesResponse (Prelude.Maybe Prelude.Text)
describeFleetAdvisorDatabasesResponse_nextToken = Lens.lens (\DescribeFleetAdvisorDatabasesResponse' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorDatabasesResponse' {} a -> s {nextToken = a} :: DescribeFleetAdvisorDatabasesResponse)

-- | The response's http status code.
describeFleetAdvisorDatabasesResponse_httpStatus :: Lens.Lens' DescribeFleetAdvisorDatabasesResponse Prelude.Int
describeFleetAdvisorDatabasesResponse_httpStatus = Lens.lens (\DescribeFleetAdvisorDatabasesResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAdvisorDatabasesResponse' {} a -> s {httpStatus = a} :: DescribeFleetAdvisorDatabasesResponse)

instance
  Prelude.NFData
    DescribeFleetAdvisorDatabasesResponse
  where
  rnf DescribeFleetAdvisorDatabasesResponse' {..} =
    Prelude.rnf databases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
