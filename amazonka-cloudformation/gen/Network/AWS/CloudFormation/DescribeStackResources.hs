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
-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resource descriptions for running and deleted stacks. If
-- @StackName@ is specified, all the associated resources that are part of
-- the stack are returned. If @PhysicalResourceId@ is specified, the
-- associated resources of the stack that the resource belongs to are
-- returned.
--
-- Only the first 100 resources will be returned. If your stack has more
-- resources than this, you should use @ListStackResources@ instead.
--
-- For deleted stacks, @DescribeStackResources@ returns resource
-- information for up to 90 days after the stack has been deleted.
--
-- You must specify either @StackName@ or @PhysicalResourceId@, but not
-- both. In addition, you can specify @LogicalResourceId@ to filter the
-- returned result. For more information about resources, the
-- @LogicalResourceId@ and @PhysicalResourceId@, go to the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/ AWS CloudFormation User Guide>.
--
-- A @ValidationError@ is returned if you specify both @StackName@ and
-- @PhysicalResourceId@ in the same request.
module Network.AWS.CloudFormation.DescribeStackResources
  ( -- * Creating a Request
    DescribeStackResources (..),
    newDescribeStackResources,

    -- * Request Lenses
    describeStackResources_stackName,
    describeStackResources_physicalResourceId,
    describeStackResources_logicalResourceId,

    -- * Destructuring the Response
    DescribeStackResourcesResponse (..),
    newDescribeStackResourcesResponse,

    -- * Response Lenses
    describeStackResourcesResponse_stackResources,
    describeStackResourcesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for DescribeStackResources action.
--
-- /See:/ 'newDescribeStackResources' smart constructor.
data DescribeStackResources = DescribeStackResources'
  { -- | The name or the unique stack ID that is associated with the stack, which
    -- are not always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    --
    -- Required: Conditional. If you do not specify @StackName@, you must
    -- specify @PhysicalResourceId@.
    stackName :: Core.Maybe Core.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by AWS CloudFormation.
    --
    -- For example, for an Amazon Elastic Compute Cloud (EC2) instance,
    -- @PhysicalResourceId@ corresponds to the @InstanceId@. You can pass the
    -- EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the
    -- instance belongs to and what other resources are part of the stack.
    --
    -- Required: Conditional. If you do not specify @PhysicalResourceId@, you
    -- must specify @StackName@.
    --
    -- Default: There is no default value.
    physicalResourceId :: Core.Maybe Core.Text,
    -- | The logical name of the resource as specified in the template.
    --
    -- Default: There is no default value.
    logicalResourceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStackResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'describeStackResources_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- Required: Conditional. If you do not specify @StackName@, you must
-- specify @PhysicalResourceId@.
--
-- 'physicalResourceId', 'describeStackResources_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance,
-- @PhysicalResourceId@ corresponds to the @InstanceId@. You can pass the
-- EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the
-- instance belongs to and what other resources are part of the stack.
--
-- Required: Conditional. If you do not specify @PhysicalResourceId@, you
-- must specify @StackName@.
--
-- Default: There is no default value.
--
-- 'logicalResourceId', 'describeStackResources_logicalResourceId' - The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
newDescribeStackResources ::
  DescribeStackResources
newDescribeStackResources =
  DescribeStackResources'
    { stackName = Core.Nothing,
      physicalResourceId = Core.Nothing,
      logicalResourceId = Core.Nothing
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
--
-- Required: Conditional. If you do not specify @StackName@, you must
-- specify @PhysicalResourceId@.
describeStackResources_stackName :: Lens.Lens' DescribeStackResources (Core.Maybe Core.Text)
describeStackResources_stackName = Lens.lens (\DescribeStackResources' {stackName} -> stackName) (\s@DescribeStackResources' {} a -> s {stackName = a} :: DescribeStackResources)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance,
-- @PhysicalResourceId@ corresponds to the @InstanceId@. You can pass the
-- EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the
-- instance belongs to and what other resources are part of the stack.
--
-- Required: Conditional. If you do not specify @PhysicalResourceId@, you
-- must specify @StackName@.
--
-- Default: There is no default value.
describeStackResources_physicalResourceId :: Lens.Lens' DescribeStackResources (Core.Maybe Core.Text)
describeStackResources_physicalResourceId = Lens.lens (\DescribeStackResources' {physicalResourceId} -> physicalResourceId) (\s@DescribeStackResources' {} a -> s {physicalResourceId = a} :: DescribeStackResources)

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
describeStackResources_logicalResourceId :: Lens.Lens' DescribeStackResources (Core.Maybe Core.Text)
describeStackResources_logicalResourceId = Lens.lens (\DescribeStackResources' {logicalResourceId} -> logicalResourceId) (\s@DescribeStackResources' {} a -> s {logicalResourceId = a} :: DescribeStackResources)

instance Core.AWSRequest DescribeStackResources where
  type
    AWSResponse DescribeStackResources =
      DescribeStackResourcesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackResourcesResult"
      ( \s h x ->
          DescribeStackResourcesResponse'
            Core.<$> ( x Core..@? "StackResources" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStackResources

instance Core.NFData DescribeStackResources

instance Core.ToHeaders DescribeStackResources where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeStackResources where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStackResources where
  toQuery DescribeStackResources' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeStackResources" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "StackName" Core.=: stackName,
        "PhysicalResourceId" Core.=: physicalResourceId,
        "LogicalResourceId" Core.=: logicalResourceId
      ]

-- | The output for a DescribeStackResources action.
--
-- /See:/ 'newDescribeStackResourcesResponse' smart constructor.
data DescribeStackResourcesResponse = DescribeStackResourcesResponse'
  { -- | A list of @StackResource@ structures.
    stackResources :: Core.Maybe [StackResource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStackResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackResources', 'describeStackResourcesResponse_stackResources' - A list of @StackResource@ structures.
--
-- 'httpStatus', 'describeStackResourcesResponse_httpStatus' - The response's http status code.
newDescribeStackResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStackResourcesResponse
newDescribeStackResourcesResponse pHttpStatus_ =
  DescribeStackResourcesResponse'
    { stackResources =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @StackResource@ structures.
describeStackResourcesResponse_stackResources :: Lens.Lens' DescribeStackResourcesResponse (Core.Maybe [StackResource])
describeStackResourcesResponse_stackResources = Lens.lens (\DescribeStackResourcesResponse' {stackResources} -> stackResources) (\s@DescribeStackResourcesResponse' {} a -> s {stackResources = a} :: DescribeStackResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStackResourcesResponse_httpStatus :: Lens.Lens' DescribeStackResourcesResponse Core.Int
describeStackResourcesResponse_httpStatus = Lens.lens (\DescribeStackResourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeStackResourcesResponse' {} a -> s {httpStatus = a} :: DescribeStackResourcesResponse)

instance Core.NFData DescribeStackResourcesResponse
