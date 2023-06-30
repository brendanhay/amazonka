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
-- Module      : Amazonka.Config.DescribeConfigurationAggregators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more configuration aggregators. If the
-- configuration aggregator is not specified, this action returns the
-- details for all the configuration aggregators associated with the
-- account.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeConfigurationAggregators
  ( -- * Creating a Request
    DescribeConfigurationAggregators (..),
    newDescribeConfigurationAggregators,

    -- * Request Lenses
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregators_limit,
    describeConfigurationAggregators_nextToken,

    -- * Destructuring the Response
    DescribeConfigurationAggregatorsResponse (..),
    newDescribeConfigurationAggregatorsResponse,

    -- * Response Lenses
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_nextToken,
    describeConfigurationAggregatorsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { -- | The name of the configuration aggregators.
    configurationAggregatorNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of configuration aggregators returned on each page.
    -- The default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationAggregators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregatorNames', 'describeConfigurationAggregators_configurationAggregatorNames' - The name of the configuration aggregators.
--
-- 'limit', 'describeConfigurationAggregators_limit' - The maximum number of configuration aggregators returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describeConfigurationAggregators_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeConfigurationAggregators ::
  DescribeConfigurationAggregators
newDescribeConfigurationAggregators =
  DescribeConfigurationAggregators'
    { configurationAggregatorNames =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the configuration aggregators.
describeConfigurationAggregators_configurationAggregatorNames :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe [Prelude.Text])
describeConfigurationAggregators_configurationAggregatorNames = Lens.lens (\DescribeConfigurationAggregators' {configurationAggregatorNames} -> configurationAggregatorNames) (\s@DescribeConfigurationAggregators' {} a -> s {configurationAggregatorNames = a} :: DescribeConfigurationAggregators) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of configuration aggregators returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
describeConfigurationAggregators_limit :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe Prelude.Natural)
describeConfigurationAggregators_limit = Lens.lens (\DescribeConfigurationAggregators' {limit} -> limit) (\s@DescribeConfigurationAggregators' {} a -> s {limit = a} :: DescribeConfigurationAggregators)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregators_nextToken :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe Prelude.Text)
describeConfigurationAggregators_nextToken = Lens.lens (\DescribeConfigurationAggregators' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregators' {} a -> s {nextToken = a} :: DescribeConfigurationAggregators)

instance
  Core.AWSPager
    DescribeConfigurationAggregators
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorsResponse_configurationAggregators
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConfigurationAggregators_nextToken
          Lens..~ rs
          Lens.^? describeConfigurationAggregatorsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeConfigurationAggregators
  where
  type
    AWSResponse DescribeConfigurationAggregators =
      DescribeConfigurationAggregatorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorsResponse'
            Prelude.<$> ( x
                            Data..?> "ConfigurationAggregators"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationAggregators
  where
  hashWithSalt
    _salt
    DescribeConfigurationAggregators' {..} =
      _salt
        `Prelude.hashWithSalt` configurationAggregatorNames
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeConfigurationAggregators
  where
  rnf DescribeConfigurationAggregators' {..} =
    Prelude.rnf configurationAggregatorNames
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeConfigurationAggregators
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConfigurationAggregators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConfigurationAggregators where
  toJSON DescribeConfigurationAggregators' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationAggregatorNames" Data..=)
              Prelude.<$> configurationAggregatorNames,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConfigurationAggregators where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeConfigurationAggregators
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { -- | Returns a ConfigurationAggregators object.
    configurationAggregators :: Prelude.Maybe [ConfigurationAggregator],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationAggregatorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregators', 'describeConfigurationAggregatorsResponse_configurationAggregators' - Returns a ConfigurationAggregators object.
--
-- 'nextToken', 'describeConfigurationAggregatorsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeConfigurationAggregatorsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationAggregatorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationAggregatorsResponse
newDescribeConfigurationAggregatorsResponse
  pHttpStatus_ =
    DescribeConfigurationAggregatorsResponse'
      { configurationAggregators =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a ConfigurationAggregators object.
describeConfigurationAggregatorsResponse_configurationAggregators :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Prelude.Maybe [ConfigurationAggregator])
describeConfigurationAggregatorsResponse_configurationAggregators = Lens.lens (\DescribeConfigurationAggregatorsResponse' {configurationAggregators} -> configurationAggregators) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {configurationAggregators = a} :: DescribeConfigurationAggregatorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregatorsResponse_nextToken :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Prelude.Maybe Prelude.Text)
describeConfigurationAggregatorsResponse_nextToken = Lens.lens (\DescribeConfigurationAggregatorsResponse' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {nextToken = a} :: DescribeConfigurationAggregatorsResponse)

-- | The response's http status code.
describeConfigurationAggregatorsResponse_httpStatus :: Lens.Lens' DescribeConfigurationAggregatorsResponse Prelude.Int
describeConfigurationAggregatorsResponse_httpStatus = Lens.lens (\DescribeConfigurationAggregatorsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationAggregatorsResponse)

instance
  Prelude.NFData
    DescribeConfigurationAggregatorsResponse
  where
  rnf DescribeConfigurationAggregatorsResponse' {..} =
    Prelude.rnf configurationAggregators
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
