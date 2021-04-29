{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.DescribeRemediationExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Config.DescribeRemediationExecutionStatus
  ( -- * Creating a Request
    DescribeRemediationExecutionStatus (..),
    newDescribeRemediationExecutionStatus,

    -- * Request Lenses
    describeRemediationExecutionStatus_nextToken,
    describeRemediationExecutionStatus_resourceKeys,
    describeRemediationExecutionStatus_limit,
    describeRemediationExecutionStatus_configRuleName,

    -- * Destructuring the Response
    DescribeRemediationExecutionStatusResponse (..),
    newDescribeRemediationExecutionStatusResponse,

    -- * Response Lenses
    describeRemediationExecutionStatusResponse_remediationExecutionStatuses,
    describeRemediationExecutionStatusResponse_nextToken,
    describeRemediationExecutionStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of resource keys to be processed with the current request. Each
    -- element in the list consists of the resource type and resource ID.
    resourceKeys :: Prelude.Maybe (Prelude.NonEmpty ResourceKey),
    -- | The maximum number of RemediationExecutionStatuses returned on each
    -- page. The default is maximum. If you specify 0, AWS Config uses the
    -- default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A list of AWS Config rule names.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRemediationExecutionStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceKeys', 'describeRemediationExecutionStatus_resourceKeys' - A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
--
-- 'limit', 'describeRemediationExecutionStatus_limit' - The maximum number of RemediationExecutionStatuses returned on each
-- page. The default is maximum. If you specify 0, AWS Config uses the
-- default.
--
-- 'configRuleName', 'describeRemediationExecutionStatus_configRuleName' - A list of AWS Config rule names.
newDescribeRemediationExecutionStatus ::
  -- | 'configRuleName'
  Prelude.Text ->
  DescribeRemediationExecutionStatus
newDescribeRemediationExecutionStatus
  pConfigRuleName_ =
    DescribeRemediationExecutionStatus'
      { nextToken =
          Prelude.Nothing,
        resourceKeys = Prelude.Nothing,
        limit = Prelude.Nothing,
        configRuleName = pConfigRuleName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatus_nextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe Prelude.Text)
describeRemediationExecutionStatus_nextToken = Lens.lens (\DescribeRemediationExecutionStatus' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatus' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatus)

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
describeRemediationExecutionStatus_resourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe (Prelude.NonEmpty ResourceKey))
describeRemediationExecutionStatus_resourceKeys = Lens.lens (\DescribeRemediationExecutionStatus' {resourceKeys} -> resourceKeys) (\s@DescribeRemediationExecutionStatus' {} a -> s {resourceKeys = a} :: DescribeRemediationExecutionStatus) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of RemediationExecutionStatuses returned on each
-- page. The default is maximum. If you specify 0, AWS Config uses the
-- default.
describeRemediationExecutionStatus_limit :: Lens.Lens' DescribeRemediationExecutionStatus (Prelude.Maybe Prelude.Natural)
describeRemediationExecutionStatus_limit = Lens.lens (\DescribeRemediationExecutionStatus' {limit} -> limit) (\s@DescribeRemediationExecutionStatus' {} a -> s {limit = a} :: DescribeRemediationExecutionStatus)

-- | A list of AWS Config rule names.
describeRemediationExecutionStatus_configRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Prelude.Text
describeRemediationExecutionStatus_configRuleName = Lens.lens (\DescribeRemediationExecutionStatus' {configRuleName} -> configRuleName) (\s@DescribeRemediationExecutionStatus' {} a -> s {configRuleName = a} :: DescribeRemediationExecutionStatus)

instance
  Pager.AWSPager
    DescribeRemediationExecutionStatus
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_remediationExecutionStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeRemediationExecutionStatus_nextToken
          Lens..~ rs
          Lens.^? describeRemediationExecutionStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeRemediationExecutionStatus
  where
  type
    Rs DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            Prelude.<$> ( x Prelude..?> "RemediationExecutionStatuses"
                            Prelude..!@ Prelude.mempty
                        )
              Prelude.<*> (x Prelude..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRemediationExecutionStatus

instance
  Prelude.NFData
    DescribeRemediationExecutionStatus

instance
  Prelude.ToHeaders
    DescribeRemediationExecutionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DescribeRemediationExecutionStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeRemediationExecutionStatus
  where
  toJSON DescribeRemediationExecutionStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("ResourceKeys" Prelude..=) Prelude.<$> resourceKeys,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just
              ("ConfigRuleName" Prelude..= configRuleName)
          ]
      )

instance
  Prelude.ToPath
    DescribeRemediationExecutionStatus
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeRemediationExecutionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { -- | Returns a list of remediation execution statuses objects.
    remediationExecutionStatuses :: Prelude.Maybe [RemediationExecutionStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExecutionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remediationExecutionStatuses', 'describeRemediationExecutionStatusResponse_remediationExecutionStatuses' - Returns a list of remediation execution statuses objects.
--
-- 'nextToken', 'describeRemediationExecutionStatusResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeRemediationExecutionStatusResponse_httpStatus' - The response's http status code.
newDescribeRemediationExecutionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRemediationExecutionStatusResponse
newDescribeRemediationExecutionStatusResponse
  pHttpStatus_ =
    DescribeRemediationExecutionStatusResponse'
      { remediationExecutionStatuses =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of remediation execution statuses objects.
describeRemediationExecutionStatusResponse_remediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Prelude.Maybe [RemediationExecutionStatus])
describeRemediationExecutionStatusResponse_remediationExecutionStatuses = Lens.lens (\DescribeRemediationExecutionStatusResponse' {remediationExecutionStatuses} -> remediationExecutionStatuses) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {remediationExecutionStatuses = a} :: DescribeRemediationExecutionStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatusResponse_nextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Prelude.Maybe Prelude.Text)
describeRemediationExecutionStatusResponse_nextToken = Lens.lens (\DescribeRemediationExecutionStatusResponse' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatusResponse)

-- | The response's http status code.
describeRemediationExecutionStatusResponse_httpStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Prelude.Int
describeRemediationExecutionStatusResponse_httpStatus = Lens.lens (\DescribeRemediationExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {httpStatus = a} :: DescribeRemediationExecutionStatusResponse)

instance
  Prelude.NFData
    DescribeRemediationExecutionStatusResponse
