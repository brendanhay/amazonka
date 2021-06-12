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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConfigurationAggregators' smart constructor.
data DescribeConfigurationAggregators = DescribeConfigurationAggregators'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the configuration aggregators.
    configurationAggregatorNames :: Core.Maybe [Core.Text],
    -- | The maximum number of configuration aggregators returned on each page.
    -- The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- The default is maximum. If you specify 0, AWS Config uses the default.
newDescribeConfigurationAggregators ::
  DescribeConfigurationAggregators
newDescribeConfigurationAggregators =
  DescribeConfigurationAggregators'
    { nextToken =
        Core.Nothing,
      configurationAggregatorNames =
        Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregators_nextToken :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe Core.Text)
describeConfigurationAggregators_nextToken = Lens.lens (\DescribeConfigurationAggregators' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregators' {} a -> s {nextToken = a} :: DescribeConfigurationAggregators)

-- | The name of the configuration aggregators.
describeConfigurationAggregators_configurationAggregatorNames :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe [Core.Text])
describeConfigurationAggregators_configurationAggregatorNames = Lens.lens (\DescribeConfigurationAggregators' {configurationAggregatorNames} -> configurationAggregatorNames) (\s@DescribeConfigurationAggregators' {} a -> s {configurationAggregatorNames = a} :: DescribeConfigurationAggregators) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of configuration aggregators returned on each page.
-- The default is maximum. If you specify 0, AWS Config uses the default.
describeConfigurationAggregators_limit :: Lens.Lens' DescribeConfigurationAggregators (Core.Maybe Core.Natural)
describeConfigurationAggregators_limit = Lens.lens (\DescribeConfigurationAggregators' {limit} -> limit) (\s@DescribeConfigurationAggregators' {} a -> s {limit = a} :: DescribeConfigurationAggregators)

instance
  Core.AWSPager
    DescribeConfigurationAggregators
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorsResponse_configurationAggregators
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeConfigurationAggregators_nextToken
          Lens..~ rs
          Lens.^? describeConfigurationAggregatorsResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ConfigurationAggregators"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeConfigurationAggregators

instance Core.NFData DescribeConfigurationAggregators

instance
  Core.ToHeaders
    DescribeConfigurationAggregators
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigurationAggregators" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConfigurationAggregators where
  toJSON DescribeConfigurationAggregators' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConfigurationAggregatorNames" Core..=)
              Core.<$> configurationAggregatorNames,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeConfigurationAggregators where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeConfigurationAggregators
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConfigurationAggregatorsResponse' smart constructor.
data DescribeConfigurationAggregatorsResponse = DescribeConfigurationAggregatorsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a ConfigurationAggregators object.
    configurationAggregators :: Core.Maybe [ConfigurationAggregator],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeConfigurationAggregatorsResponse
newDescribeConfigurationAggregatorsResponse
  pHttpStatus_ =
    DescribeConfigurationAggregatorsResponse'
      { nextToken =
          Core.Nothing,
        configurationAggregators =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregatorsResponse_nextToken :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Core.Maybe Core.Text)
describeConfigurationAggregatorsResponse_nextToken = Lens.lens (\DescribeConfigurationAggregatorsResponse' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {nextToken = a} :: DescribeConfigurationAggregatorsResponse)

-- | Returns a ConfigurationAggregators object.
describeConfigurationAggregatorsResponse_configurationAggregators :: Lens.Lens' DescribeConfigurationAggregatorsResponse (Core.Maybe [ConfigurationAggregator])
describeConfigurationAggregatorsResponse_configurationAggregators = Lens.lens (\DescribeConfigurationAggregatorsResponse' {configurationAggregators} -> configurationAggregators) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {configurationAggregators = a} :: DescribeConfigurationAggregatorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigurationAggregatorsResponse_httpStatus :: Lens.Lens' DescribeConfigurationAggregatorsResponse Core.Int
describeConfigurationAggregatorsResponse_httpStatus = Lens.lens (\DescribeConfigurationAggregatorsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationAggregatorsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationAggregatorsResponse)

instance
  Core.NFData
    DescribeConfigurationAggregatorsResponse
