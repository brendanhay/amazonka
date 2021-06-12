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
-- Module      : Network.AWS.ResourceGroups.GroupResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified resources to the specified group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:GroupResources@
module Network.AWS.ResourceGroups.GroupResources
  ( -- * Creating a Request
    GroupResources (..),
    newGroupResources,

    -- * Request Lenses
    groupResources_group,
    groupResources_resourceArns,

    -- * Destructuring the Response
    GroupResourcesResponse (..),
    newGroupResourcesResponse,

    -- * Response Lenses
    groupResourcesResponse_succeeded,
    groupResourcesResponse_pending,
    groupResourcesResponse_failed,
    groupResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGroupResources' smart constructor.
data GroupResources = GroupResources'
  { -- | The name or the ARN of the resource group to add resources to.
    group' :: Core.Text,
    -- | The list of ARNs for resources to be added to the group.
    resourceArns :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'groupResources_group' - The name or the ARN of the resource group to add resources to.
--
-- 'resourceArns', 'groupResources_resourceArns' - The list of ARNs for resources to be added to the group.
newGroupResources ::
  -- | 'group''
  Core.Text ->
  -- | 'resourceArns'
  Core.NonEmpty Core.Text ->
  GroupResources
newGroupResources pGroup_ pResourceArns_ =
  GroupResources'
    { group' = pGroup_,
      resourceArns = Lens._Coerce Lens.# pResourceArns_
    }

-- | The name or the ARN of the resource group to add resources to.
groupResources_group :: Lens.Lens' GroupResources Core.Text
groupResources_group = Lens.lens (\GroupResources' {group'} -> group') (\s@GroupResources' {} a -> s {group' = a} :: GroupResources)

-- | The list of ARNs for resources to be added to the group.
groupResources_resourceArns :: Lens.Lens' GroupResources (Core.NonEmpty Core.Text)
groupResources_resourceArns = Lens.lens (\GroupResources' {resourceArns} -> resourceArns) (\s@GroupResources' {} a -> s {resourceArns = a} :: GroupResources) Core.. Lens._Coerce

instance Core.AWSRequest GroupResources where
  type
    AWSResponse GroupResources =
      GroupResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GroupResourcesResponse'
            Core.<$> (x Core..?> "Succeeded")
            Core.<*> (x Core..?> "Pending" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Failed" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GroupResources

instance Core.NFData GroupResources

instance Core.ToHeaders GroupResources where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GroupResources where
  toJSON GroupResources' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Group" Core..= group'),
            Core.Just ("ResourceArns" Core..= resourceArns)
          ]
      )

instance Core.ToPath GroupResources where
  toPath = Core.const "/group-resources"

instance Core.ToQuery GroupResources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGroupResourcesResponse' smart constructor.
data GroupResourcesResponse = GroupResourcesResponse'
  { -- | A list of ARNs of resources that were successfully added to the group by
    -- this operation.
    succeeded :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of ARNs of any resources that are still in the process of being
    -- added to the group by this operation. These pending additions continue
    -- asynchronously. You can check the status of pending additions by using
    -- the @ ListGroupResources @ operation, and checking the @Resources@ array
    -- in the response and the @Status@ field of each object in that array.
    pending :: Core.Maybe [PendingResource],
    -- | A list of ARNs of any resources that failed to be added to the group by
    -- this operation.
    failed :: Core.Maybe [FailedResource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeeded', 'groupResourcesResponse_succeeded' - A list of ARNs of resources that were successfully added to the group by
-- this operation.
--
-- 'pending', 'groupResourcesResponse_pending' - A list of ARNs of any resources that are still in the process of being
-- added to the group by this operation. These pending additions continue
-- asynchronously. You can check the status of pending additions by using
-- the @ ListGroupResources @ operation, and checking the @Resources@ array
-- in the response and the @Status@ field of each object in that array.
--
-- 'failed', 'groupResourcesResponse_failed' - A list of ARNs of any resources that failed to be added to the group by
-- this operation.
--
-- 'httpStatus', 'groupResourcesResponse_httpStatus' - The response's http status code.
newGroupResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GroupResourcesResponse
newGroupResourcesResponse pHttpStatus_ =
  GroupResourcesResponse'
    { succeeded = Core.Nothing,
      pending = Core.Nothing,
      failed = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ARNs of resources that were successfully added to the group by
-- this operation.
groupResourcesResponse_succeeded :: Lens.Lens' GroupResourcesResponse (Core.Maybe (Core.NonEmpty Core.Text))
groupResourcesResponse_succeeded = Lens.lens (\GroupResourcesResponse' {succeeded} -> succeeded) (\s@GroupResourcesResponse' {} a -> s {succeeded = a} :: GroupResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of ARNs of any resources that are still in the process of being
-- added to the group by this operation. These pending additions continue
-- asynchronously. You can check the status of pending additions by using
-- the @ ListGroupResources @ operation, and checking the @Resources@ array
-- in the response and the @Status@ field of each object in that array.
groupResourcesResponse_pending :: Lens.Lens' GroupResourcesResponse (Core.Maybe [PendingResource])
groupResourcesResponse_pending = Lens.lens (\GroupResourcesResponse' {pending} -> pending) (\s@GroupResourcesResponse' {} a -> s {pending = a} :: GroupResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of ARNs of any resources that failed to be added to the group by
-- this operation.
groupResourcesResponse_failed :: Lens.Lens' GroupResourcesResponse (Core.Maybe [FailedResource])
groupResourcesResponse_failed = Lens.lens (\GroupResourcesResponse' {failed} -> failed) (\s@GroupResourcesResponse' {} a -> s {failed = a} :: GroupResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
groupResourcesResponse_httpStatus :: Lens.Lens' GroupResourcesResponse Core.Int
groupResourcesResponse_httpStatus = Lens.lens (\GroupResourcesResponse' {httpStatus} -> httpStatus) (\s@GroupResourcesResponse' {} a -> s {httpStatus = a} :: GroupResourcesResponse)

instance Core.NFData GroupResourcesResponse
