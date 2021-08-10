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
-- Module      : Network.AWS.ResourceGroups.UngroupResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified resources from the specified group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:UngroupResources@
module Network.AWS.ResourceGroups.UngroupResources
  ( -- * Creating a Request
    UngroupResources (..),
    newUngroupResources,

    -- * Request Lenses
    ungroupResources_group,
    ungroupResources_resourceArns,

    -- * Destructuring the Response
    UngroupResourcesResponse (..),
    newUngroupResourcesResponse,

    -- * Response Lenses
    ungroupResourcesResponse_succeeded,
    ungroupResourcesResponse_pending,
    ungroupResourcesResponse_failed,
    ungroupResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUngroupResources' smart constructor.
data UngroupResources = UngroupResources'
  { -- | The name or the ARN of the resource group from which to remove the
    -- resources.
    group' :: Prelude.Text,
    -- | The ARNs of the resources to be removed from the group.
    resourceArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UngroupResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'ungroupResources_group' - The name or the ARN of the resource group from which to remove the
-- resources.
--
-- 'resourceArns', 'ungroupResources_resourceArns' - The ARNs of the resources to be removed from the group.
newUngroupResources ::
  -- | 'group''
  Prelude.Text ->
  -- | 'resourceArns'
  Prelude.NonEmpty Prelude.Text ->
  UngroupResources
newUngroupResources pGroup_ pResourceArns_ =
  UngroupResources'
    { group' = pGroup_,
      resourceArns = Lens._Coerce Lens.# pResourceArns_
    }

-- | The name or the ARN of the resource group from which to remove the
-- resources.
ungroupResources_group :: Lens.Lens' UngroupResources Prelude.Text
ungroupResources_group = Lens.lens (\UngroupResources' {group'} -> group') (\s@UngroupResources' {} a -> s {group' = a} :: UngroupResources)

-- | The ARNs of the resources to be removed from the group.
ungroupResources_resourceArns :: Lens.Lens' UngroupResources (Prelude.NonEmpty Prelude.Text)
ungroupResources_resourceArns = Lens.lens (\UngroupResources' {resourceArns} -> resourceArns) (\s@UngroupResources' {} a -> s {resourceArns = a} :: UngroupResources) Prelude.. Lens._Coerce

instance Core.AWSRequest UngroupResources where
  type
    AWSResponse UngroupResources =
      UngroupResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UngroupResourcesResponse'
            Prelude.<$> (x Core..?> "Succeeded")
            Prelude.<*> (x Core..?> "Pending" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Failed" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UngroupResources

instance Prelude.NFData UngroupResources

instance Core.ToHeaders UngroupResources where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UngroupResources where
  toJSON UngroupResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Group" Core..= group'),
            Prelude.Just ("ResourceArns" Core..= resourceArns)
          ]
      )

instance Core.ToPath UngroupResources where
  toPath = Prelude.const "/ungroup-resources"

instance Core.ToQuery UngroupResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUngroupResourcesResponse' smart constructor.
data UngroupResourcesResponse = UngroupResourcesResponse'
  { -- | A list of resources that were successfully removed from the group by
    -- this operation.
    succeeded :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of any resources that are still in the process of being removed
    -- from the group by this operation. These pending removals continue
    -- asynchronously. You can check the status of pending removals by using
    -- the @ ListGroupResources @ operation. After the resource is successfully
    -- removed, it no longer appears in the response.
    pending :: Prelude.Maybe [PendingResource],
    -- | A list of any resources that failed to be removed from the group by this
    -- operation.
    failed :: Prelude.Maybe [FailedResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UngroupResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'succeeded', 'ungroupResourcesResponse_succeeded' - A list of resources that were successfully removed from the group by
-- this operation.
--
-- 'pending', 'ungroupResourcesResponse_pending' - A list of any resources that are still in the process of being removed
-- from the group by this operation. These pending removals continue
-- asynchronously. You can check the status of pending removals by using
-- the @ ListGroupResources @ operation. After the resource is successfully
-- removed, it no longer appears in the response.
--
-- 'failed', 'ungroupResourcesResponse_failed' - A list of any resources that failed to be removed from the group by this
-- operation.
--
-- 'httpStatus', 'ungroupResourcesResponse_httpStatus' - The response's http status code.
newUngroupResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UngroupResourcesResponse
newUngroupResourcesResponse pHttpStatus_ =
  UngroupResourcesResponse'
    { succeeded =
        Prelude.Nothing,
      pending = Prelude.Nothing,
      failed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of resources that were successfully removed from the group by
-- this operation.
ungroupResourcesResponse_succeeded :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
ungroupResourcesResponse_succeeded = Lens.lens (\UngroupResourcesResponse' {succeeded} -> succeeded) (\s@UngroupResourcesResponse' {} a -> s {succeeded = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of any resources that are still in the process of being removed
-- from the group by this operation. These pending removals continue
-- asynchronously. You can check the status of pending removals by using
-- the @ ListGroupResources @ operation. After the resource is successfully
-- removed, it no longer appears in the response.
ungroupResourcesResponse_pending :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe [PendingResource])
ungroupResourcesResponse_pending = Lens.lens (\UngroupResourcesResponse' {pending} -> pending) (\s@UngroupResourcesResponse' {} a -> s {pending = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of any resources that failed to be removed from the group by this
-- operation.
ungroupResourcesResponse_failed :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe [FailedResource])
ungroupResourcesResponse_failed = Lens.lens (\UngroupResourcesResponse' {failed} -> failed) (\s@UngroupResourcesResponse' {} a -> s {failed = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
ungroupResourcesResponse_httpStatus :: Lens.Lens' UngroupResourcesResponse Prelude.Int
ungroupResourcesResponse_httpStatus = Lens.lens (\UngroupResourcesResponse' {httpStatus} -> httpStatus) (\s@UngroupResourcesResponse' {} a -> s {httpStatus = a} :: UngroupResourcesResponse)

instance Prelude.NFData UngroupResourcesResponse
