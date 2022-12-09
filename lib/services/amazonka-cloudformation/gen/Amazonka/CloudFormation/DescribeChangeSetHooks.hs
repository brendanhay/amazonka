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
-- Module      : Amazonka.CloudFormation.DescribeChangeSetHooks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns hook-related information for the change set and a list of
-- changes that CloudFormation makes when you run the change set.
module Amazonka.CloudFormation.DescribeChangeSetHooks
  ( -- * Creating a Request
    DescribeChangeSetHooks (..),
    newDescribeChangeSetHooks,

    -- * Request Lenses
    describeChangeSetHooks_logicalResourceId,
    describeChangeSetHooks_nextToken,
    describeChangeSetHooks_stackName,
    describeChangeSetHooks_changeSetName,

    -- * Destructuring the Response
    DescribeChangeSetHooksResponse (..),
    newDescribeChangeSetHooksResponse,

    -- * Response Lenses
    describeChangeSetHooksResponse_changeSetId,
    describeChangeSetHooksResponse_changeSetName,
    describeChangeSetHooksResponse_hooks,
    describeChangeSetHooksResponse_nextToken,
    describeChangeSetHooksResponse_stackId,
    describeChangeSetHooksResponse_stackName,
    describeChangeSetHooksResponse_status,
    describeChangeSetHooksResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChangeSetHooks' smart constructor.
data DescribeChangeSetHooks = DescribeChangeSetHooks'
  { -- | If specified, lists only the hooks related to the specified
    -- @LogicalResourceId@.
    logicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | A string, provided by the @DescribeChangeSetHooks@ response output, that
    -- identifies the next page of information that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If you specified the name of a change set, specify the stack name or
    -- stack ID (ARN) of the change set you want to describe.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the change set that you want
    -- to describe.
    changeSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSetHooks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceId', 'describeChangeSetHooks_logicalResourceId' - If specified, lists only the hooks related to the specified
-- @LogicalResourceId@.
--
-- 'nextToken', 'describeChangeSetHooks_nextToken' - A string, provided by the @DescribeChangeSetHooks@ response output, that
-- identifies the next page of information that you want to retrieve.
--
-- 'stackName', 'describeChangeSetHooks_stackName' - If you specified the name of a change set, specify the stack name or
-- stack ID (ARN) of the change set you want to describe.
--
-- 'changeSetName', 'describeChangeSetHooks_changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want
-- to describe.
newDescribeChangeSetHooks ::
  -- | 'changeSetName'
  Prelude.Text ->
  DescribeChangeSetHooks
newDescribeChangeSetHooks pChangeSetName_ =
  DescribeChangeSetHooks'
    { logicalResourceId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stackName = Prelude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | If specified, lists only the hooks related to the specified
-- @LogicalResourceId@.
describeChangeSetHooks_logicalResourceId :: Lens.Lens' DescribeChangeSetHooks (Prelude.Maybe Prelude.Text)
describeChangeSetHooks_logicalResourceId = Lens.lens (\DescribeChangeSetHooks' {logicalResourceId} -> logicalResourceId) (\s@DescribeChangeSetHooks' {} a -> s {logicalResourceId = a} :: DescribeChangeSetHooks)

-- | A string, provided by the @DescribeChangeSetHooks@ response output, that
-- identifies the next page of information that you want to retrieve.
describeChangeSetHooks_nextToken :: Lens.Lens' DescribeChangeSetHooks (Prelude.Maybe Prelude.Text)
describeChangeSetHooks_nextToken = Lens.lens (\DescribeChangeSetHooks' {nextToken} -> nextToken) (\s@DescribeChangeSetHooks' {} a -> s {nextToken = a} :: DescribeChangeSetHooks)

-- | If you specified the name of a change set, specify the stack name or
-- stack ID (ARN) of the change set you want to describe.
describeChangeSetHooks_stackName :: Lens.Lens' DescribeChangeSetHooks (Prelude.Maybe Prelude.Text)
describeChangeSetHooks_stackName = Lens.lens (\DescribeChangeSetHooks' {stackName} -> stackName) (\s@DescribeChangeSetHooks' {} a -> s {stackName = a} :: DescribeChangeSetHooks)

-- | The name or Amazon Resource Name (ARN) of the change set that you want
-- to describe.
describeChangeSetHooks_changeSetName :: Lens.Lens' DescribeChangeSetHooks Prelude.Text
describeChangeSetHooks_changeSetName = Lens.lens (\DescribeChangeSetHooks' {changeSetName} -> changeSetName) (\s@DescribeChangeSetHooks' {} a -> s {changeSetName = a} :: DescribeChangeSetHooks)

instance Core.AWSRequest DescribeChangeSetHooks where
  type
    AWSResponse DescribeChangeSetHooks =
      DescribeChangeSetHooksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeChangeSetHooksResult"
      ( \s h x ->
          DescribeChangeSetHooksResponse'
            Prelude.<$> (x Data..@? "ChangeSetId")
            Prelude.<*> (x Data..@? "ChangeSetName")
            Prelude.<*> ( x Data..@? "Hooks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (x Data..@? "StackId")
            Prelude.<*> (x Data..@? "StackName")
            Prelude.<*> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChangeSetHooks where
  hashWithSalt _salt DescribeChangeSetHooks' {..} =
    _salt `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` changeSetName

instance Prelude.NFData DescribeChangeSetHooks where
  rnf DescribeChangeSetHooks' {..} =
    Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf changeSetName

instance Data.ToHeaders DescribeChangeSetHooks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeChangeSetHooks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeChangeSetHooks where
  toQuery DescribeChangeSetHooks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeChangeSetHooks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "LogicalResourceId" Data.=: logicalResourceId,
        "NextToken" Data.=: nextToken,
        "StackName" Data.=: stackName,
        "ChangeSetName" Data.=: changeSetName
      ]

-- | /See:/ 'newDescribeChangeSetHooksResponse' smart constructor.
data DescribeChangeSetHooksResponse = DescribeChangeSetHooksResponse'
  { -- | The change set identifier (stack ID).
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The change set name.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | List of hook objects.
    hooks :: Prelude.Maybe [ChangeSetHook],
    -- | Pagination token, @null@ or empty if no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The stack identifier (stack ID).
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The stack name.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the change set hook.
    status :: Prelude.Maybe ChangeSetHooksStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChangeSetHooksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetId', 'describeChangeSetHooksResponse_changeSetId' - The change set identifier (stack ID).
--
-- 'changeSetName', 'describeChangeSetHooksResponse_changeSetName' - The change set name.
--
-- 'hooks', 'describeChangeSetHooksResponse_hooks' - List of hook objects.
--
-- 'nextToken', 'describeChangeSetHooksResponse_nextToken' - Pagination token, @null@ or empty if no more results.
--
-- 'stackId', 'describeChangeSetHooksResponse_stackId' - The stack identifier (stack ID).
--
-- 'stackName', 'describeChangeSetHooksResponse_stackName' - The stack name.
--
-- 'status', 'describeChangeSetHooksResponse_status' - Provides the status of the change set hook.
--
-- 'httpStatus', 'describeChangeSetHooksResponse_httpStatus' - The response's http status code.
newDescribeChangeSetHooksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChangeSetHooksResponse
newDescribeChangeSetHooksResponse pHttpStatus_ =
  DescribeChangeSetHooksResponse'
    { changeSetId =
        Prelude.Nothing,
      changeSetName = Prelude.Nothing,
      hooks = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stackId = Prelude.Nothing,
      stackName = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The change set identifier (stack ID).
describeChangeSetHooksResponse_changeSetId :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe Prelude.Text)
describeChangeSetHooksResponse_changeSetId = Lens.lens (\DescribeChangeSetHooksResponse' {changeSetId} -> changeSetId) (\s@DescribeChangeSetHooksResponse' {} a -> s {changeSetId = a} :: DescribeChangeSetHooksResponse)

-- | The change set name.
describeChangeSetHooksResponse_changeSetName :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe Prelude.Text)
describeChangeSetHooksResponse_changeSetName = Lens.lens (\DescribeChangeSetHooksResponse' {changeSetName} -> changeSetName) (\s@DescribeChangeSetHooksResponse' {} a -> s {changeSetName = a} :: DescribeChangeSetHooksResponse)

-- | List of hook objects.
describeChangeSetHooksResponse_hooks :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe [ChangeSetHook])
describeChangeSetHooksResponse_hooks = Lens.lens (\DescribeChangeSetHooksResponse' {hooks} -> hooks) (\s@DescribeChangeSetHooksResponse' {} a -> s {hooks = a} :: DescribeChangeSetHooksResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token, @null@ or empty if no more results.
describeChangeSetHooksResponse_nextToken :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe Prelude.Text)
describeChangeSetHooksResponse_nextToken = Lens.lens (\DescribeChangeSetHooksResponse' {nextToken} -> nextToken) (\s@DescribeChangeSetHooksResponse' {} a -> s {nextToken = a} :: DescribeChangeSetHooksResponse)

-- | The stack identifier (stack ID).
describeChangeSetHooksResponse_stackId :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe Prelude.Text)
describeChangeSetHooksResponse_stackId = Lens.lens (\DescribeChangeSetHooksResponse' {stackId} -> stackId) (\s@DescribeChangeSetHooksResponse' {} a -> s {stackId = a} :: DescribeChangeSetHooksResponse)

-- | The stack name.
describeChangeSetHooksResponse_stackName :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe Prelude.Text)
describeChangeSetHooksResponse_stackName = Lens.lens (\DescribeChangeSetHooksResponse' {stackName} -> stackName) (\s@DescribeChangeSetHooksResponse' {} a -> s {stackName = a} :: DescribeChangeSetHooksResponse)

-- | Provides the status of the change set hook.
describeChangeSetHooksResponse_status :: Lens.Lens' DescribeChangeSetHooksResponse (Prelude.Maybe ChangeSetHooksStatus)
describeChangeSetHooksResponse_status = Lens.lens (\DescribeChangeSetHooksResponse' {status} -> status) (\s@DescribeChangeSetHooksResponse' {} a -> s {status = a} :: DescribeChangeSetHooksResponse)

-- | The response's http status code.
describeChangeSetHooksResponse_httpStatus :: Lens.Lens' DescribeChangeSetHooksResponse Prelude.Int
describeChangeSetHooksResponse_httpStatus = Lens.lens (\DescribeChangeSetHooksResponse' {httpStatus} -> httpStatus) (\s@DescribeChangeSetHooksResponse' {} a -> s {httpStatus = a} :: DescribeChangeSetHooksResponse)

instance
  Prelude.NFData
    DescribeChangeSetHooksResponse
  where
  rnf DescribeChangeSetHooksResponse' {..} =
    Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf hooks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
