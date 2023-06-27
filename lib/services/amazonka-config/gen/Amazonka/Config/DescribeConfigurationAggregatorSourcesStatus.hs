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
-- Module      : Amazonka.Config.DescribeConfigurationAggregatorSourcesStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for sources within an aggregator. The status
-- includes information about the last time Config verified authorization
-- between the source account and an aggregator account. In case of a
-- failure, the status contains the related error code or message.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeConfigurationAggregatorSourcesStatus
  ( -- * Creating a Request
    DescribeConfigurationAggregatorSourcesStatus (..),
    newDescribeConfigurationAggregatorSourcesStatus,

    -- * Request Lenses
    describeConfigurationAggregatorSourcesStatus_limit,
    describeConfigurationAggregatorSourcesStatus_nextToken,
    describeConfigurationAggregatorSourcesStatus_updateStatus,
    describeConfigurationAggregatorSourcesStatus_configurationAggregatorName,

    -- * Destructuring the Response
    DescribeConfigurationAggregatorSourcesStatusResponse (..),
    newDescribeConfigurationAggregatorSourcesStatusResponse,

    -- * Response Lenses
    describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList,
    describeConfigurationAggregatorSourcesStatusResponse_nextToken,
    describeConfigurationAggregatorSourcesStatusResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfigurationAggregatorSourcesStatus' smart constructor.
data DescribeConfigurationAggregatorSourcesStatus = DescribeConfigurationAggregatorSourcesStatus'
  { -- | The maximum number of AggregatorSourceStatus returned on each page. The
    -- default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the status type.
    --
    -- -   Valid value FAILED indicates errors while moving data.
    --
    -- -   Valid value SUCCEEDED indicates the data was successfully moved.
    --
    -- -   Valid value OUTDATED indicates the data is not the most recent.
    updateStatus :: Prelude.Maybe (Prelude.NonEmpty AggregatedSourceStatusType),
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationAggregatorSourcesStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeConfigurationAggregatorSourcesStatus_limit' - The maximum number of AggregatorSourceStatus returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describeConfigurationAggregatorSourcesStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'updateStatus', 'describeConfigurationAggregatorSourcesStatus_updateStatus' - Filters the status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
--
-- 'configurationAggregatorName', 'describeConfigurationAggregatorSourcesStatus_configurationAggregatorName' - The name of the configuration aggregator.
newDescribeConfigurationAggregatorSourcesStatus ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  DescribeConfigurationAggregatorSourcesStatus
newDescribeConfigurationAggregatorSourcesStatus
  pConfigurationAggregatorName_ =
    DescribeConfigurationAggregatorSourcesStatus'
      { limit =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        updateStatus =
          Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The maximum number of AggregatorSourceStatus returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
describeConfigurationAggregatorSourcesStatus_limit :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Prelude.Maybe Prelude.Natural)
describeConfigurationAggregatorSourcesStatus_limit = Lens.lens (\DescribeConfigurationAggregatorSourcesStatus' {limit} -> limit) (\s@DescribeConfigurationAggregatorSourcesStatus' {} a -> s {limit = a} :: DescribeConfigurationAggregatorSourcesStatus)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregatorSourcesStatus_nextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Prelude.Maybe Prelude.Text)
describeConfigurationAggregatorSourcesStatus_nextToken = Lens.lens (\DescribeConfigurationAggregatorSourcesStatus' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregatorSourcesStatus' {} a -> s {nextToken = a} :: DescribeConfigurationAggregatorSourcesStatus)

-- | Filters the status type.
--
-- -   Valid value FAILED indicates errors while moving data.
--
-- -   Valid value SUCCEEDED indicates the data was successfully moved.
--
-- -   Valid value OUTDATED indicates the data is not the most recent.
describeConfigurationAggregatorSourcesStatus_updateStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Prelude.Maybe (Prelude.NonEmpty AggregatedSourceStatusType))
describeConfigurationAggregatorSourcesStatus_updateStatus = Lens.lens (\DescribeConfigurationAggregatorSourcesStatus' {updateStatus} -> updateStatus) (\s@DescribeConfigurationAggregatorSourcesStatus' {} a -> s {updateStatus = a} :: DescribeConfigurationAggregatorSourcesStatus) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configuration aggregator.
describeConfigurationAggregatorSourcesStatus_configurationAggregatorName :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus Prelude.Text
describeConfigurationAggregatorSourcesStatus_configurationAggregatorName = Lens.lens (\DescribeConfigurationAggregatorSourcesStatus' {configurationAggregatorName} -> configurationAggregatorName) (\s@DescribeConfigurationAggregatorSourcesStatus' {} a -> s {configurationAggregatorName = a} :: DescribeConfigurationAggregatorSourcesStatus)

instance
  Core.AWSPager
    DescribeConfigurationAggregatorSourcesStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorSourcesStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConfigurationAggregatorSourcesStatus_nextToken
          Lens..~ rs
          Lens.^? describeConfigurationAggregatorSourcesStatusResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeConfigurationAggregatorSourcesStatus
  where
  type
    AWSResponse
      DescribeConfigurationAggregatorSourcesStatus =
      DescribeConfigurationAggregatorSourcesStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorSourcesStatusResponse'
            Prelude.<$> ( x
                            Data..?> "AggregatedSourceStatusList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationAggregatorSourcesStatus
  where
  hashWithSalt
    _salt
    DescribeConfigurationAggregatorSourcesStatus' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` updateStatus
        `Prelude.hashWithSalt` configurationAggregatorName

instance
  Prelude.NFData
    DescribeConfigurationAggregatorSourcesStatus
  where
  rnf DescribeConfigurationAggregatorSourcesStatus' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf configurationAggregatorName

instance
  Data.ToHeaders
    DescribeConfigurationAggregatorSourcesStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConfigurationAggregatorSourcesStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeConfigurationAggregatorSourcesStatus
  where
  toJSON
    DescribeConfigurationAggregatorSourcesStatus' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Limit" Data..=) Prelude.<$> limit,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              ("UpdateStatus" Data..=) Prelude.<$> updateStatus,
              Prelude.Just
                ( "ConfigurationAggregatorName"
                    Data..= configurationAggregatorName
                )
            ]
        )

instance
  Data.ToPath
    DescribeConfigurationAggregatorSourcesStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeConfigurationAggregatorSourcesStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationAggregatorSourcesStatusResponse' smart constructor.
data DescribeConfigurationAggregatorSourcesStatusResponse = DescribeConfigurationAggregatorSourcesStatusResponse'
  { -- | Returns an AggregatedSourceStatus object.
    aggregatedSourceStatusList :: Prelude.Maybe [AggregatedSourceStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationAggregatorSourcesStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregatedSourceStatusList', 'describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList' - Returns an AggregatedSourceStatus object.
--
-- 'nextToken', 'describeConfigurationAggregatorSourcesStatusResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeConfigurationAggregatorSourcesStatusResponse_httpStatus' - The response's http status code.
newDescribeConfigurationAggregatorSourcesStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationAggregatorSourcesStatusResponse
newDescribeConfigurationAggregatorSourcesStatusResponse
  pHttpStatus_ =
    DescribeConfigurationAggregatorSourcesStatusResponse'
      { aggregatedSourceStatusList =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns an AggregatedSourceStatus object.
describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Prelude.Maybe [AggregatedSourceStatus])
describeConfigurationAggregatorSourcesStatusResponse_aggregatedSourceStatusList = Lens.lens (\DescribeConfigurationAggregatorSourcesStatusResponse' {aggregatedSourceStatusList} -> aggregatedSourceStatusList) (\s@DescribeConfigurationAggregatorSourcesStatusResponse' {} a -> s {aggregatedSourceStatusList = a} :: DescribeConfigurationAggregatorSourcesStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigurationAggregatorSourcesStatusResponse_nextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Prelude.Maybe Prelude.Text)
describeConfigurationAggregatorSourcesStatusResponse_nextToken = Lens.lens (\DescribeConfigurationAggregatorSourcesStatusResponse' {nextToken} -> nextToken) (\s@DescribeConfigurationAggregatorSourcesStatusResponse' {} a -> s {nextToken = a} :: DescribeConfigurationAggregatorSourcesStatusResponse)

-- | The response's http status code.
describeConfigurationAggregatorSourcesStatusResponse_httpStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse Prelude.Int
describeConfigurationAggregatorSourcesStatusResponse_httpStatus = Lens.lens (\DescribeConfigurationAggregatorSourcesStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationAggregatorSourcesStatusResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationAggregatorSourcesStatusResponse)

instance
  Prelude.NFData
    DescribeConfigurationAggregatorSourcesStatusResponse
  where
  rnf
    DescribeConfigurationAggregatorSourcesStatusResponse' {..} =
      Prelude.rnf aggregatedSourceStatusList
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
