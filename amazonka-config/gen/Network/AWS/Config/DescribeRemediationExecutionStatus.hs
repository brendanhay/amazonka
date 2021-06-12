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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRemediationExecutionStatus' smart constructor.
data DescribeRemediationExecutionStatus = DescribeRemediationExecutionStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of resource keys to be processed with the current request. Each
    -- element in the list consists of the resource type and resource ID.
    resourceKeys :: Core.Maybe (Core.NonEmpty ResourceKey),
    -- | The maximum number of RemediationExecutionStatuses returned on each
    -- page. The default is maximum. If you specify 0, AWS Config uses the
    -- default.
    limit :: Core.Maybe Core.Natural,
    -- | A list of AWS Config rule names.
    configRuleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeRemediationExecutionStatus
newDescribeRemediationExecutionStatus
  pConfigRuleName_ =
    DescribeRemediationExecutionStatus'
      { nextToken =
          Core.Nothing,
        resourceKeys = Core.Nothing,
        limit = Core.Nothing,
        configRuleName = pConfigRuleName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatus_nextToken :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Core.Text)
describeRemediationExecutionStatus_nextToken = Lens.lens (\DescribeRemediationExecutionStatus' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatus' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatus)

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
describeRemediationExecutionStatus_resourceKeys :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe (Core.NonEmpty ResourceKey))
describeRemediationExecutionStatus_resourceKeys = Lens.lens (\DescribeRemediationExecutionStatus' {resourceKeys} -> resourceKeys) (\s@DescribeRemediationExecutionStatus' {} a -> s {resourceKeys = a} :: DescribeRemediationExecutionStatus) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of RemediationExecutionStatuses returned on each
-- page. The default is maximum. If you specify 0, AWS Config uses the
-- default.
describeRemediationExecutionStatus_limit :: Lens.Lens' DescribeRemediationExecutionStatus (Core.Maybe Core.Natural)
describeRemediationExecutionStatus_limit = Lens.lens (\DescribeRemediationExecutionStatus' {limit} -> limit) (\s@DescribeRemediationExecutionStatus' {} a -> s {limit = a} :: DescribeRemediationExecutionStatus)

-- | A list of AWS Config rule names.
describeRemediationExecutionStatus_configRuleName :: Lens.Lens' DescribeRemediationExecutionStatus Core.Text
describeRemediationExecutionStatus_configRuleName = Lens.lens (\DescribeRemediationExecutionStatus' {configRuleName} -> configRuleName) (\s@DescribeRemediationExecutionStatus' {} a -> s {configRuleName = a} :: DescribeRemediationExecutionStatus)

instance
  Core.AWSPager
    DescribeRemediationExecutionStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRemediationExecutionStatusResponse_remediationExecutionStatuses
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeRemediationExecutionStatus_nextToken
          Lens..~ rs
          Lens.^? describeRemediationExecutionStatusResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeRemediationExecutionStatus
  where
  type
    AWSResponse DescribeRemediationExecutionStatus =
      DescribeRemediationExecutionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationExecutionStatusResponse'
            Core.<$> ( x Core..?> "RemediationExecutionStatuses"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeRemediationExecutionStatus

instance
  Core.NFData
    DescribeRemediationExecutionStatus

instance
  Core.ToHeaders
    DescribeRemediationExecutionStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeRemediationExecutionStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeRemediationExecutionStatus
  where
  toJSON DescribeRemediationExecutionStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceKeys" Core..=) Core.<$> resourceKeys,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("ConfigRuleName" Core..= configRuleName)
          ]
      )

instance
  Core.ToPath
    DescribeRemediationExecutionStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeRemediationExecutionStatus
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRemediationExecutionStatusResponse' smart constructor.
data DescribeRemediationExecutionStatusResponse = DescribeRemediationExecutionStatusResponse'
  { -- | Returns a list of remediation execution statuses objects.
    remediationExecutionStatuses :: Core.Maybe [RemediationExecutionStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeRemediationExecutionStatusResponse
newDescribeRemediationExecutionStatusResponse
  pHttpStatus_ =
    DescribeRemediationExecutionStatusResponse'
      { remediationExecutionStatuses =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of remediation execution statuses objects.
describeRemediationExecutionStatusResponse_remediationExecutionStatuses :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe [RemediationExecutionStatus])
describeRemediationExecutionStatusResponse_remediationExecutionStatuses = Lens.lens (\DescribeRemediationExecutionStatusResponse' {remediationExecutionStatuses} -> remediationExecutionStatuses) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {remediationExecutionStatuses = a} :: DescribeRemediationExecutionStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRemediationExecutionStatusResponse_nextToken :: Lens.Lens' DescribeRemediationExecutionStatusResponse (Core.Maybe Core.Text)
describeRemediationExecutionStatusResponse_nextToken = Lens.lens (\DescribeRemediationExecutionStatusResponse' {nextToken} -> nextToken) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {nextToken = a} :: DescribeRemediationExecutionStatusResponse)

-- | The response's http status code.
describeRemediationExecutionStatusResponse_httpStatus :: Lens.Lens' DescribeRemediationExecutionStatusResponse Core.Int
describeRemediationExecutionStatusResponse_httpStatus = Lens.lens (\DescribeRemediationExecutionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationExecutionStatusResponse' {} a -> s {httpStatus = a} :: DescribeRemediationExecutionStatusResponse)

instance
  Core.NFData
    DescribeRemediationExecutionStatusResponse
