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
-- Module      : Amazonka.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed view of a Remediation Execution for a set of
-- resources including state, timestamps for when steps for the remediation
-- execution occur, and any error messages for steps that have failed. When
-- you specify the limit and the next token, you receive a paginated
-- response.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeRemediationExecutionStatus
  ( -- * Creating a Request
    DescribeRemediationExecutionStatus (..),
    newDescribeRemediationExecutionStatus,

    -- * Request Lenses
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_configRuleName,

    -- * Destructuring the Response
    DescribeRemediationExecutionStatusResponse (..),
    newDescribeRemediationExecutionStatusResponse,

    -- * Response Lenses
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { -- | The maximum number of RemediationExecutionStatuses returned on each
    -- page. The default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of resource keys to be processed with the current request. Each
    -- element in the list consists of the resource type and resource ID.
    resourceKeys :: Prelude.Maybe (Prelude.NonEmpty ResourceKey),
    -- | A list of Config rule names.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeRemediationExecutionStatus_limit' - The maximum number of RemediationExecutionStatuses returned on each
-- page. The default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describeRemediationExecutionStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceKeys', 'describeRemediationExecutionStatus_resourceKeys' - A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
--
-- 'configRuleName', 'describeRemediationExecutionStatus_configRuleName' - A list of Config rule names.
newDescribeRemediationExecutionStatus ::
  -- | 'configRuleName'
  Prelude.Text ->
  DescribeRemediationExecutionStatus
newDescribeRemediationExecutionStatus
  pConfigRuleName_ =
    DescribeRemediationExecutionStatus'
      { limit =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        resourceKeys = Prelude.Nothing,
        configRuleName = pConfigRuleName_
      }

-- | The maximum number of RemediationExecutionStatuses returned on each
-- page. The default is maximum. If you specify 0, Config uses the default.
describeRemediationExecutionStatus_limit :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe Prelude.Natural)
describeRemediationExecutionStatus_limit = Lens.lens (\DescribeRemediationExecutionStatus' {limit} -> limit) (\s@DescribeRemediationExecutionStatus' {} a -> s {limit = a} :: DescribeRemediationExecutionStatus)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatus_nextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe Prelude.Text)
describeRemediationExecutionStatus_nextToken = Lens.lens (\DescribeRemediationExecutionStatus' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatus' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatus)

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
describeRemediationExecutionStatus_resourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe (Prelude.NonEmpty ResourceKey))
describeRemediationExecutionStatus_resourceKeys = Lens.lens (\DescribeRemediationExecutionStatus' {resourceKeys} -> resourceKeys) (\s@DescribeRemediationExecutionStatus' {} a -> s {resourceKeys = a} :: DescribeRemediationExecutionStatus) Prelude.. Lens.mapping Lens.coerced

-- | A list of Config rule names.
describeRemediationExecutionStatus_configRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Prelude.Text
describeRemediationExecutionStatus_configRuleName = Lens.lens (\DescribeRemediationExecutionStatus' {configRuleName} -> configRuleName) (\s@DescribeRemediationExecutionStatus' {} a -> s {configRuleName = a} :: DescribeRemediationExecutionStatus)

instance
  Core.AWSPager
    DescribeRemediationExecutionStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_remediationExecutionStatuses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeRemediationExecutionStatus_nextToken
          Lens..~ rs
          Lens.^? describeRemediationExecutionStatusResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeRemediationExecutionStatus
  where
  type
    AWSResponse DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RemediationExecutionStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRemediationExecutionStatus
  where
  hashWithSalt
    _salt
    DescribeRemediationExecutionStatus' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resourceKeys
        `Prelude.hashWithSalt` configRuleName

instance
  Prelude.NFData
    DescribeRemediationExecutionStatus
  where
  rnf DescribeRemediationExecutionStatus' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceKeys
      `Prelude.seq` Prelude.rnf configRuleName

instance
  Data.ToHeaders
    DescribeRemediationExecutionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeRemediationExecutionStatus" ::
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
    DescribeRemediationExecutionStatus
  where
  toJSON DescribeRemediationExecutionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceKeys" Data..=) Prelude.<$> resourceKeys,
            Prelude.Just
              ("ConfigRuleName" Data..= configRuleName)
          ]
      )

instance
  Data.ToPath
    DescribeRemediationExecutionStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeRemediationExecutionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of remediation execution statuses objects.
    remediationExecutionStatuses :: Prelude.Maybe [RemediationExecutionStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExecutionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRemediationExecutionStatusResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'remediationExecutionStatuses', 'describeRemediationExecutionStatusResponse_remediationExecutionStatuses' - Returns a list of remediation execution statuses objects.
--
-- 'httpStatus', 'describeRemediationExecutionStatusResponse_httpStatus' - The response's http status code.
newDescribeRemediationExecutionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRemediationExecutionStatusResponse
newDescribeRemediationExecutionStatusResponse
  pHttpStatus_ =
    DescribeRemediationExecutionStatusResponse'
      { nextToken =
          Prelude.Nothing,
        remediationExecutionStatuses =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatusResponse_nextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Prelude.Maybe Prelude.Text)
describeRemediationExecutionStatusResponse_nextToken = Lens.lens (\DescribeRemediationExecutionStatusResponse' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatusResponse)

-- | Returns a list of remediation execution statuses objects.
describeRemediationExecutionStatusResponse_remediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Prelude.Maybe [RemediationExecutionStatus])
describeRemediationExecutionStatusResponse_remediationExecutionStatuses = Lens.lens (\DescribeRemediationExecutionStatusResponse' {remediationExecutionStatuses} -> remediationExecutionStatuses) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {remediationExecutionStatuses = a} :: DescribeRemediationExecutionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRemediationExecutionStatusResponse_httpStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Prelude.Int
describeRemediationExecutionStatusResponse_httpStatus = Lens.lens (\DescribeRemediationExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {httpStatus = a} :: DescribeRemediationExecutionStatusResponse)

instance
  Prelude.NFData
    DescribeRemediationExecutionStatusResponse
  where
  rnf DescribeRemediationExecutionStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf remediationExecutionStatuses
      `Prelude.seq` Prelude.rnf httpStatus
