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
-- Module      : Amazonka.ResourceGroups.UngroupResources
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ResourceGroups.UngroupResources
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
    ungroupResourcesResponse_failed,
    ungroupResourcesResponse_pending,
    ungroupResourcesResponse_succeeded,
    ungroupResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

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
      resourceArns = Lens.coerced Lens.# pResourceArns_
    }

-- | The name or the ARN of the resource group from which to remove the
-- resources.
ungroupResources_group :: Lens.Lens' UngroupResources Prelude.Text
ungroupResources_group = Lens.lens (\UngroupResources' {group'} -> group') (\s@UngroupResources' {} a -> s {group' = a} :: UngroupResources)

-- | The ARNs of the resources to be removed from the group.
ungroupResources_resourceArns :: Lens.Lens' UngroupResources (Prelude.NonEmpty Prelude.Text)
ungroupResources_resourceArns = Lens.lens (\UngroupResources' {resourceArns} -> resourceArns) (\s@UngroupResources' {} a -> s {resourceArns = a} :: UngroupResources) Prelude.. Lens.coerced

instance Core.AWSRequest UngroupResources where
  type
    AWSResponse UngroupResources =
      UngroupResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UngroupResourcesResponse'
            Prelude.<$> (x Data..?> "Failed" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Pending" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Succeeded")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UngroupResources where
  hashWithSalt _salt UngroupResources' {..} =
    _salt
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` resourceArns

instance Prelude.NFData UngroupResources where
  rnf UngroupResources' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf resourceArns

instance Data.ToHeaders UngroupResources where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UngroupResources where
  toJSON UngroupResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Group" Data..= group'),
            Prelude.Just ("ResourceArns" Data..= resourceArns)
          ]
      )

instance Data.ToPath UngroupResources where
  toPath = Prelude.const "/ungroup-resources"

instance Data.ToQuery UngroupResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUngroupResourcesResponse' smart constructor.
data UngroupResourcesResponse = UngroupResourcesResponse'
  { -- | A list of any resources that failed to be removed from the group by this
    -- operation.
    failed :: Prelude.Maybe [FailedResource],
    -- | A list of any resources that are still in the process of being removed
    -- from the group by this operation. These pending removals continue
    -- asynchronously. You can check the status of pending removals by using
    -- the @ @@ListGroupResources@@ @ operation. After the resource is
    -- successfully removed, it no longer appears in the response.
    pending :: Prelude.Maybe [PendingResource],
    -- | A list of resources that were successfully removed from the group by
    -- this operation.
    succeeded :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'failed', 'ungroupResourcesResponse_failed' - A list of any resources that failed to be removed from the group by this
-- operation.
--
-- 'pending', 'ungroupResourcesResponse_pending' - A list of any resources that are still in the process of being removed
-- from the group by this operation. These pending removals continue
-- asynchronously. You can check the status of pending removals by using
-- the @ @@ListGroupResources@@ @ operation. After the resource is
-- successfully removed, it no longer appears in the response.
--
-- 'succeeded', 'ungroupResourcesResponse_succeeded' - A list of resources that were successfully removed from the group by
-- this operation.
--
-- 'httpStatus', 'ungroupResourcesResponse_httpStatus' - The response's http status code.
newUngroupResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UngroupResourcesResponse
newUngroupResourcesResponse pHttpStatus_ =
  UngroupResourcesResponse'
    { failed = Prelude.Nothing,
      pending = Prelude.Nothing,
      succeeded = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of any resources that failed to be removed from the group by this
-- operation.
ungroupResourcesResponse_failed :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe [FailedResource])
ungroupResourcesResponse_failed = Lens.lens (\UngroupResourcesResponse' {failed} -> failed) (\s@UngroupResourcesResponse' {} a -> s {failed = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of any resources that are still in the process of being removed
-- from the group by this operation. These pending removals continue
-- asynchronously. You can check the status of pending removals by using
-- the @ @@ListGroupResources@@ @ operation. After the resource is
-- successfully removed, it no longer appears in the response.
ungroupResourcesResponse_pending :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe [PendingResource])
ungroupResourcesResponse_pending = Lens.lens (\UngroupResourcesResponse' {pending} -> pending) (\s@UngroupResourcesResponse' {} a -> s {pending = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of resources that were successfully removed from the group by
-- this operation.
ungroupResourcesResponse_succeeded :: Lens.Lens' UngroupResourcesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
ungroupResourcesResponse_succeeded = Lens.lens (\UngroupResourcesResponse' {succeeded} -> succeeded) (\s@UngroupResourcesResponse' {} a -> s {succeeded = a} :: UngroupResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
ungroupResourcesResponse_httpStatus :: Lens.Lens' UngroupResourcesResponse Prelude.Int
ungroupResourcesResponse_httpStatus = Lens.lens (\UngroupResourcesResponse' {httpStatus} -> httpStatus) (\s@UngroupResourcesResponse' {} a -> s {httpStatus = a} :: UngroupResourcesResponse)

instance Prelude.NFData UngroupResourcesResponse where
  rnf UngroupResourcesResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf pending
      `Prelude.seq` Prelude.rnf succeeded
      `Prelude.seq` Prelude.rnf httpStatus
