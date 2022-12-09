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
-- Module      : Amazonka.DMS.DescribeFleetAdvisorCollectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Fleet Advisor collectors in your account.
module Amazonka.DMS.DescribeFleetAdvisorCollectors
  ( -- * Creating a Request
    DescribeFleetAdvisorCollectors (..),
    newDescribeFleetAdvisorCollectors,

    -- * Request Lenses
    describeFleetAdvisorCollectors_filters,
    describeFleetAdvisorCollectors_maxRecords,
    describeFleetAdvisorCollectors_nextToken,

    -- * Destructuring the Response
    DescribeFleetAdvisorCollectorsResponse (..),
    newDescribeFleetAdvisorCollectorsResponse,

    -- * Response Lenses
    describeFleetAdvisorCollectorsResponse_collectors,
    describeFleetAdvisorCollectorsResponse_nextToken,
    describeFleetAdvisorCollectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetAdvisorCollectors' smart constructor.
data DescribeFleetAdvisorCollectors = DescribeFleetAdvisorCollectors'
  { -- | If you specify any of the following filters, the output includes
    -- information for only those collectors that meet the filter criteria:
    --
    -- -   @collector-referenced-id@ – The ID of the collector agent, for
    --     example @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
    --
    -- -   @collector-name@ – The name of the collector agent.
    --
    -- An example is:
    -- @describe-fleet-advisor-collectors --filter Name=\"collector-referenced-id\",Values=\"d4610ac5-e323-4ad9-bc50-eaf7249dfe9d\"@
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
-- Create a value of 'DescribeFleetAdvisorCollectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeFleetAdvisorCollectors_filters' - If you specify any of the following filters, the output includes
-- information for only those collectors that meet the filter criteria:
--
-- -   @collector-referenced-id@ – The ID of the collector agent, for
--     example @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
--
-- -   @collector-name@ – The name of the collector agent.
--
-- An example is:
-- @describe-fleet-advisor-collectors --filter Name=\"collector-referenced-id\",Values=\"d4610ac5-e323-4ad9-bc50-eaf7249dfe9d\"@
--
-- 'maxRecords', 'describeFleetAdvisorCollectors_maxRecords' - Sets the maximum number of records returned in the response.
--
-- 'nextToken', 'describeFleetAdvisorCollectors_nextToken' - If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
newDescribeFleetAdvisorCollectors ::
  DescribeFleetAdvisorCollectors
newDescribeFleetAdvisorCollectors =
  DescribeFleetAdvisorCollectors'
    { filters =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | If you specify any of the following filters, the output includes
-- information for only those collectors that meet the filter criteria:
--
-- -   @collector-referenced-id@ – The ID of the collector agent, for
--     example @d4610ac5-e323-4ad9-bc50-eaf7249dfe9d@.
--
-- -   @collector-name@ – The name of the collector agent.
--
-- An example is:
-- @describe-fleet-advisor-collectors --filter Name=\"collector-referenced-id\",Values=\"d4610ac5-e323-4ad9-bc50-eaf7249dfe9d\"@
describeFleetAdvisorCollectors_filters :: Lens.Lens' DescribeFleetAdvisorCollectors (Prelude.Maybe [Filter])
describeFleetAdvisorCollectors_filters = Lens.lens (\DescribeFleetAdvisorCollectors' {filters} -> filters) (\s@DescribeFleetAdvisorCollectors' {} a -> s {filters = a} :: DescribeFleetAdvisorCollectors) Prelude.. Lens.mapping Lens.coerced

-- | Sets the maximum number of records returned in the response.
describeFleetAdvisorCollectors_maxRecords :: Lens.Lens' DescribeFleetAdvisorCollectors (Prelude.Maybe Prelude.Int)
describeFleetAdvisorCollectors_maxRecords = Lens.lens (\DescribeFleetAdvisorCollectors' {maxRecords} -> maxRecords) (\s@DescribeFleetAdvisorCollectors' {} a -> s {maxRecords = a} :: DescribeFleetAdvisorCollectors)

-- | If @NextToken@ is returned by a previous response, there are more
-- results available. The value of @NextToken@ is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeFleetAdvisorCollectors_nextToken :: Lens.Lens' DescribeFleetAdvisorCollectors (Prelude.Maybe Prelude.Text)
describeFleetAdvisorCollectors_nextToken = Lens.lens (\DescribeFleetAdvisorCollectors' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorCollectors' {} a -> s {nextToken = a} :: DescribeFleetAdvisorCollectors)

instance
  Core.AWSRequest
    DescribeFleetAdvisorCollectors
  where
  type
    AWSResponse DescribeFleetAdvisorCollectors =
      DescribeFleetAdvisorCollectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetAdvisorCollectorsResponse'
            Prelude.<$> (x Data..?> "Collectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFleetAdvisorCollectors
  where
  hashWithSalt
    _salt
    DescribeFleetAdvisorCollectors' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeFleetAdvisorCollectors
  where
  rnf DescribeFleetAdvisorCollectors' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeFleetAdvisorCollectors
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeFleetAdvisorCollectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeFleetAdvisorCollectors where
  toJSON DescribeFleetAdvisorCollectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeFleetAdvisorCollectors where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetAdvisorCollectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetAdvisorCollectorsResponse' smart constructor.
data DescribeFleetAdvisorCollectorsResponse = DescribeFleetAdvisorCollectorsResponse'
  { -- | Provides descriptions of the Fleet Advisor collectors, including the
    -- collectors\' name and ID, and the latest inventory data.
    collectors :: Prelude.Maybe [CollectorResponse],
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
-- Create a value of 'DescribeFleetAdvisorCollectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectors', 'describeFleetAdvisorCollectorsResponse_collectors' - Provides descriptions of the Fleet Advisor collectors, including the
-- collectors\' name and ID, and the latest inventory data.
--
-- 'nextToken', 'describeFleetAdvisorCollectorsResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'httpStatus', 'describeFleetAdvisorCollectorsResponse_httpStatus' - The response's http status code.
newDescribeFleetAdvisorCollectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetAdvisorCollectorsResponse
newDescribeFleetAdvisorCollectorsResponse
  pHttpStatus_ =
    DescribeFleetAdvisorCollectorsResponse'
      { collectors =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Provides descriptions of the Fleet Advisor collectors, including the
-- collectors\' name and ID, and the latest inventory data.
describeFleetAdvisorCollectorsResponse_collectors :: Lens.Lens' DescribeFleetAdvisorCollectorsResponse (Prelude.Maybe [CollectorResponse])
describeFleetAdvisorCollectorsResponse_collectors = Lens.lens (\DescribeFleetAdvisorCollectorsResponse' {collectors} -> collectors) (\s@DescribeFleetAdvisorCollectorsResponse' {} a -> s {collectors = a} :: DescribeFleetAdvisorCollectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
describeFleetAdvisorCollectorsResponse_nextToken :: Lens.Lens' DescribeFleetAdvisorCollectorsResponse (Prelude.Maybe Prelude.Text)
describeFleetAdvisorCollectorsResponse_nextToken = Lens.lens (\DescribeFleetAdvisorCollectorsResponse' {nextToken} -> nextToken) (\s@DescribeFleetAdvisorCollectorsResponse' {} a -> s {nextToken = a} :: DescribeFleetAdvisorCollectorsResponse)

-- | The response's http status code.
describeFleetAdvisorCollectorsResponse_httpStatus :: Lens.Lens' DescribeFleetAdvisorCollectorsResponse Prelude.Int
describeFleetAdvisorCollectorsResponse_httpStatus = Lens.lens (\DescribeFleetAdvisorCollectorsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetAdvisorCollectorsResponse' {} a -> s {httpStatus = a} :: DescribeFleetAdvisorCollectorsResponse)

instance
  Prelude.NFData
    DescribeFleetAdvisorCollectorsResponse
  where
  rnf DescribeFleetAdvisorCollectorsResponse' {..} =
    Prelude.rnf collectors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
