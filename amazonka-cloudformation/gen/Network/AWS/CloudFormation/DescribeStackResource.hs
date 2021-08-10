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
-- Module      : Network.AWS.CloudFormation.DescribeStackResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified resource in the specified stack.
--
-- For deleted stacks, DescribeStackResource returns resource information
-- for up to 90 days after the stack has been deleted.
module Network.AWS.CloudFormation.DescribeStackResource
  ( -- * Creating a Request
    DescribeStackResource (..),
    newDescribeStackResource,

    -- * Request Lenses
    describeStackResource_stackName,
    describeStackResource_logicalResourceId,

    -- * Destructuring the Response
    DescribeStackResourceResponse (..),
    newDescribeStackResourceResponse,

    -- * Response Lenses
    describeStackResourceResponse_stackResourceDetail,
    describeStackResourceResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for DescribeStackResource action.
--
-- /See:/ 'newDescribeStackResource' smart constructor.
data DescribeStackResource = DescribeStackResource'
  { -- | The name or the unique stack ID that is associated with the stack, which
    -- are not always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Prelude.Text,
    -- | The logical name of the resource as specified in the template.
    --
    -- Default: There is no default value.
    logicalResourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'describeStackResource_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- 'logicalResourceId', 'describeStackResource_logicalResourceId' - The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
newDescribeStackResource ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'logicalResourceId'
  Prelude.Text ->
  DescribeStackResource
newDescribeStackResource
  pStackName_
  pLogicalResourceId_ =
    DescribeStackResource'
      { stackName = pStackName_,
        logicalResourceId = pLogicalResourceId_
      }

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
describeStackResource_stackName :: Lens.Lens' DescribeStackResource Prelude.Text
describeStackResource_stackName = Lens.lens (\DescribeStackResource' {stackName} -> stackName) (\s@DescribeStackResource' {} a -> s {stackName = a} :: DescribeStackResource)

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
describeStackResource_logicalResourceId :: Lens.Lens' DescribeStackResource Prelude.Text
describeStackResource_logicalResourceId = Lens.lens (\DescribeStackResource' {logicalResourceId} -> logicalResourceId) (\s@DescribeStackResource' {} a -> s {logicalResourceId = a} :: DescribeStackResource)

instance Core.AWSRequest DescribeStackResource where
  type
    AWSResponse DescribeStackResource =
      DescribeStackResourceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackResourceResult"
      ( \s h x ->
          DescribeStackResourceResponse'
            Prelude.<$> (x Core..@? "StackResourceDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackResource

instance Prelude.NFData DescribeStackResource

instance Core.ToHeaders DescribeStackResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStackResource where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStackResource where
  toQuery DescribeStackResource' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStackResource" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Core.=: stackName,
        "LogicalResourceId" Core.=: logicalResourceId
      ]

-- | The output for a DescribeStackResource action.
--
-- /See:/ 'newDescribeStackResourceResponse' smart constructor.
data DescribeStackResourceResponse = DescribeStackResourceResponse'
  { -- | A @StackResourceDetail@ structure containing the description of the
    -- specified resource in the specified stack.
    stackResourceDetail :: Prelude.Maybe StackResourceDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackResourceDetail', 'describeStackResourceResponse_stackResourceDetail' - A @StackResourceDetail@ structure containing the description of the
-- specified resource in the specified stack.
--
-- 'httpStatus', 'describeStackResourceResponse_httpStatus' - The response's http status code.
newDescribeStackResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackResourceResponse
newDescribeStackResourceResponse pHttpStatus_ =
  DescribeStackResourceResponse'
    { stackResourceDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @StackResourceDetail@ structure containing the description of the
-- specified resource in the specified stack.
describeStackResourceResponse_stackResourceDetail :: Lens.Lens' DescribeStackResourceResponse (Prelude.Maybe StackResourceDetail)
describeStackResourceResponse_stackResourceDetail = Lens.lens (\DescribeStackResourceResponse' {stackResourceDetail} -> stackResourceDetail) (\s@DescribeStackResourceResponse' {} a -> s {stackResourceDetail = a} :: DescribeStackResourceResponse)

-- | The response's http status code.
describeStackResourceResponse_httpStatus :: Lens.Lens' DescribeStackResourceResponse Prelude.Int
describeStackResourceResponse_httpStatus = Lens.lens (\DescribeStackResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeStackResourceResponse' {} a -> s {httpStatus = a} :: DescribeStackResourceResponse)

instance Prelude.NFData DescribeStackResourceResponse
