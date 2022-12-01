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
-- Module      : Amazonka.DMS.DescribeFleetAdvisorSchemaObjectSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides descriptions of the schemas discovered by your Fleet Advisor
-- collectors.
module Amazonka.DMS.DescribeFleetAdvisorSchemaObjectSummary
  ( -- * Creating a Request
    DescribeFleetAdvisorSchemaObjectSummary (..),
    newDescribeFleetAdvisorSchemaObjectSummary,

    -- * Request Lenses
    describeFleetAdvisorSchemaObjectSummary_nextToken,
    describeFleetAdvisorSchemaObjectSummary_filters,
    describeFleetAdvisorSchemaObjectSummary_maxRecords,

    -- * Destructuring the Response
    DescribeFleetAdvisorSchemaObjectSummaryResponse (..),
    newDescribeFleetAdvisorSchemaObjectSummaryResponse,

    -- * Response Lenses
    describeFleetAdvisorSchemaObjectSummaryResponse_nextToken,
    describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects,
    describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetAdvisorSchemaObjectSummary' smart constructor.
data DescribeFleetAdvisorSchemaObjectSummary = DescribeFleetAdvisorSchemaObjectSummary'
  { -- | If @NextToken@ is returned by a previous response, there are more
    -- results available. The value of @NextToken@ is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If you specify any of the following filters, the output includes
    -- information for only those schema objects that meet the filter criteria:
    --
    -- -   @schema-id@ – The ID of the schema, for example
    --     @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
    --
    -- Example:
    -- @describe-fleet-advisor-schema-object-summary --filter Name=\"schema-id\",Values=\"50\"@
    filters :: Prelude.Maybe [Filter],
    -- | Sets the maximum number of records returned in the response.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAdvisorSchemaObjectSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetAdvisorSchemaObjectSummary_nextToken' - If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'filters', 'describeFleetAdvisorSchemaObjectSummary_filters' - If you specify any of the following filters, the output includes
-- information for only those schema objects that meet the filter criteria:
--
-- -   @schema-id@ – The ID of the schema, for example
--     @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
--
-- Example:
-- @describe-fleet-advisor-schema-object-summary --filter Name=\"schema-id\",Values=\"50\"@
--
-- 'maxRecords', 'describeFleetAdvisorSchemaObjectSummary_maxRecords' - Sets the maximum number of records returned in the response.
newDescribeFleetAdvisorSchemaObjectSummary ::
  DescribeFleetAdvisorSchemaObjectSummary
newDescribeFleetAdvisorSchemaObjectSummary =
  DescribeFleetAdvisorSchemaObjectSummary'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeFleetAdvisorSchemaObjectSummary_nextToken :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummary (Prelude.Maybe Prelude.Text)
describeFleetAdvisorSchemaObjectSummary_nextToken = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummary' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorSchemaObjectSummary' {} a -> s {nextToken = a} :: DescribeFleetAdvisorSchemaObjectSummary)

-- | If you specify any of the following filters, the output includes
-- information for only those schema objects that meet the filter criteria:
--
-- -   @schema-id@ – The ID of the schema, for example
--     @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
--
-- Example:
-- @describe-fleet-advisor-schema-object-summary --filter Name=\"schema-id\",Values=\"50\"@
describeFleetAdvisorSchemaObjectSummary_filters :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummary (Prelude.Maybe [Filter])
describeFleetAdvisorSchemaObjectSummary_filters = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummary' {filters} -> filters) (\s@DescribeFleetAdvisorSchemaObjectSummary' {} a -> s {filters = a} :: DescribeFleetAdvisorSchemaObjectSummary) Prelude.. Lens.mapping Lens.coerced

-- | Sets the maximum number of records returned in the response.
describeFleetAdvisorSchemaObjectSummary_maxRecords :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummary (Prelude.Maybe Prelude.Int)
describeFleetAdvisorSchemaObjectSummary_maxRecords = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummary' {maxRecords} -> maxRecords) (\s@DescribeFleetAdvisorSchemaObjectSummary' {} a -> s {maxRecords = a} :: DescribeFleetAdvisorSchemaObjectSummary)

instance
  Core.AWSRequest
    DescribeFleetAdvisorSchemaObjectSummary
  where
  type
    AWSResponse
      DescribeFleetAdvisorSchemaObjectSummary =
      DescribeFleetAdvisorSchemaObjectSummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAdvisorSchemaObjectSummaryResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "FleetAdvisorSchemaObjects"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFleetAdvisorSchemaObjectSummary
  where
  hashWithSalt
    _salt
    DescribeFleetAdvisorSchemaObjectSummary' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeFleetAdvisorSchemaObjectSummary
  where
  rnf DescribeFleetAdvisorSchemaObjectSummary' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Core.ToHeaders
    DescribeFleetAdvisorSchemaObjectSummary
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeFleetAdvisorSchemaObjectSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeFleetAdvisorSchemaObjectSummary
  where
  toJSON DescribeFleetAdvisorSchemaObjectSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance
  Core.ToPath
    DescribeFleetAdvisorSchemaObjectSummary
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeFleetAdvisorSchemaObjectSummary
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetAdvisorSchemaObjectSummaryResponse' smart constructor.
data DescribeFleetAdvisorSchemaObjectSummaryResponse = DescribeFleetAdvisorSchemaObjectSummaryResponse'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of @FleetAdvisorSchemaObjectResponse@ objects.
    fleetAdvisorSchemaObjects :: Prelude.Maybe [FleetAdvisorSchemaObjectResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetAdvisorSchemaObjectSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetAdvisorSchemaObjectSummaryResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'fleetAdvisorSchemaObjects', 'describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects' - A collection of @FleetAdvisorSchemaObjectResponse@ objects.
--
-- 'httpStatus', 'describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus' - The response's http status code.
newDescribeFleetAdvisorSchemaObjectSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetAdvisorSchemaObjectSummaryResponse
newDescribeFleetAdvisorSchemaObjectSummaryResponse
  pHttpStatus_ =
    DescribeFleetAdvisorSchemaObjectSummaryResponse'
      { nextToken =
          Prelude.Nothing,
        fleetAdvisorSchemaObjects =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
describeFleetAdvisorSchemaObjectSummaryResponse_nextToken :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummaryResponse (Prelude.Maybe Prelude.Text)
describeFleetAdvisorSchemaObjectSummaryResponse_nextToken = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummaryResponse' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorSchemaObjectSummaryResponse' {} a -> s {nextToken = a} :: DescribeFleetAdvisorSchemaObjectSummaryResponse)

-- | A collection of @FleetAdvisorSchemaObjectResponse@ objects.
describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummaryResponse (Prelude.Maybe [FleetAdvisorSchemaObjectResponse])
describeFleetAdvisorSchemaObjectSummaryResponse_fleetAdvisorSchemaObjects = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummaryResponse' {fleetAdvisorSchemaObjects} -> fleetAdvisorSchemaObjects) (\s@DescribeFleetAdvisorSchemaObjectSummaryResponse' {} a -> s {fleetAdvisorSchemaObjects = a} :: DescribeFleetAdvisorSchemaObjectSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus :: Lens.Lens' DescribeFleetAdvisorSchemaObjectSummaryResponse Prelude.Int
describeFleetAdvisorSchemaObjectSummaryResponse_httpStatus = Lens.lens (\DescribeFleetAdvisorSchemaObjectSummaryResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAdvisorSchemaObjectSummaryResponse' {} a -> s {httpStatus = a} :: DescribeFleetAdvisorSchemaObjectSummaryResponse)

instance
  Prelude.NFData
    DescribeFleetAdvisorSchemaObjectSummaryResponse
  where
  rnf
    DescribeFleetAdvisorSchemaObjectSummaryResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf fleetAdvisorSchemaObjects
        `Prelude.seq` Prelude.rnf httpStatus
