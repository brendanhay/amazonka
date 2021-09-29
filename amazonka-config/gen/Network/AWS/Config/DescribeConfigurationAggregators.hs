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
-- Module      : Network.AWS.Config.DescribeConfigurationAggregators
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Config.DescribeConfigurationAggregators
  ( -- * Creating a Request
    DescribeConfigurationAggregators (..),
    newDescribeConfigurationAggregators,

    -- * Request Lenses
    describeConfigurationAggregators_nextToken,
    describeConfigurationAggregators_configurationAggregatorNames,
    describeConfigurationAggregators_limit,

    -- * Destructuring the Response
    DescribeConfigurationAggregatorsResponse (..),
    newDescribeConfigurationAggregatorsResponse,

    -- * Response Lenses
    describeConfigurationAggregatorsResponse_nextToken,
    describeConfigurationAggregatorsResponse_configurationAggregators,
    describeConfigurationAggregatorsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregators.
    configurationAggregatorNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of configuration aggregators returned on each page.
    -- The default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeConfigurationAggregators_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorNames', 'describeConfigurationAggregators_configurationAggregatorNames' - The name of the configuration aggregators.
--
-- 'limit', 'describeConfigurationAggregators_limit' - The maximum number of configuration aggregators returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
newDescribeConfigurationAggregators ::
  DescribeConfigurationAggregators
newDescribeConfigurationAggregators =
  DescribeConfigurationAggregators'
    { nextToken =
        Prelude.Nothing,
      configurationAggregatorNames =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregators_nextToken :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe Prelude.Text)
describeConfigurationAggregators_nextToken = Lens.lens (\DescribeConfigurationAggregators' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregators' {} a -> s {nextToken = a} :: DescribeConfigurationAggregators)

-- | The name of the configuration aggregators.
describeConfigurationAggregators_configurationAggregatorNames :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe [Prelude.Text])
describeConfigurationAggregators_configurationAggregatorNames = Lens.lens (\DescribeConfigurationAggregators' {configurationAggregatorNames} -> configurationAggregatorNames) (\s@DescribeConfigurationAggregators' {} a -> s {configurationAggregatorNames = a} :: DescribeConfigurationAggregators) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of configuration aggregators returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
describeConfigurationAggregators_limit :: Lens.Lens' DescribeConfigurationAggregators (Prelude.Maybe Prelude.Natural)
describeConfigurationAggregators_limit = Lens.lens (\DescribeConfigurationAggregators' {limit} -> limit) (\s@DescribeConfigurationAggregators' {} a -> s {limit = a} :: DescribeConfigurationAggregators)

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
      Prelude.Just Prelude.$
        rq
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ConfigurationAggregators"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationAggregators

instance
  Prelude.NFData
    DescribeConfigurationAggregators

instance
  Core.ToHeaders
    DescribeConfigurationAggregators
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigurationAggregators" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConfigurationAggregators where
  toJSON DescribeConfigurationAggregators' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ConfigurationAggregatorNames" Core..=)
              Prelude.<$> configurationAggregatorNames,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeConfigurationAggregators where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeConfigurationAggregators
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a ConfigurationAggregators object.
    configurationAggregators :: Prelude.Maybe [ConfigurationAggregator],
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
-- 'nextToken', 'describeConfigurationAggregatorsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregators', 'describeConfigurationAggregatorsResponse_configurationAggregators' - Returns a ConfigurationAggregators object.
--
-- 'httpStatus', 'describeConfigurationAggregatorsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationAggregatorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationAggregatorsResponse
newDescribeConfigurationAggregatorsResponse
  pHttpStatus_ =
    DescribeConfigurationAggregatorsResponse'
      { nextToken =
          Prelude.Nothing,
        configurationAggregators =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregatorsResponse_nextToken :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Prelude.Maybe Prelude.Text)
describeConfigurationAggregatorsResponse_nextToken = Lens.lens (\DescribeConfigurationAggregatorsResponse' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {nextToken = a} :: DescribeConfigurationAggregatorsResponse)

-- | Returns a ConfigurationAggregators object.
describeConfigurationAggregatorsResponse_configurationAggregators :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Prelude.Maybe [ConfigurationAggregator])
describeConfigurationAggregatorsResponse_configurationAggregators = Lens.lens (\DescribeConfigurationAggregatorsResponse' {configurationAggregators} -> configurationAggregators) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {configurationAggregators = a} :: DescribeConfigurationAggregatorsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigurationAggregatorsResponse_httpStatus :: Lens.Lens' DescribeConfigurationAggregatorsResponse Prelude.Int
describeConfigurationAggregatorsResponse_httpStatus = Lens.lens (\DescribeConfigurationAggregatorsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationAggregatorsResponse)

instance
  Prelude.NFData
    DescribeConfigurationAggregatorsResponse
