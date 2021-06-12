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
-- Module      : Network.AWS.IoT.DeleteDynamicThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dynamic thing group.
module Network.AWS.IoT.DeleteDynamicThingGroup
  ( -- * Creating a Request
    DeleteDynamicThingGroup (..),
    newDeleteDynamicThingGroup,

    -- * Request Lenses
    deleteDynamicThingGroup_expectedVersion,
    deleteDynamicThingGroup_thingGroupName,

    -- * Destructuring the Response
    DeleteDynamicThingGroupResponse (..),
    newDeleteDynamicThingGroupResponse,

    -- * Response Lenses
    deleteDynamicThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDynamicThingGroup' smart constructor.
data DeleteDynamicThingGroup = DeleteDynamicThingGroup'
  { -- | The expected version of the dynamic thing group to delete.
    expectedVersion :: Core.Maybe Core.Integer,
    -- | The name of the dynamic thing group to delete.
    thingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDynamicThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteDynamicThingGroup_expectedVersion' - The expected version of the dynamic thing group to delete.
--
-- 'thingGroupName', 'deleteDynamicThingGroup_thingGroupName' - The name of the dynamic thing group to delete.
newDeleteDynamicThingGroup ::
  -- | 'thingGroupName'
  Core.Text ->
  DeleteDynamicThingGroup
newDeleteDynamicThingGroup pThingGroupName_ =
  DeleteDynamicThingGroup'
    { expectedVersion =
        Core.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the dynamic thing group to delete.
deleteDynamicThingGroup_expectedVersion :: Lens.Lens' DeleteDynamicThingGroup (Core.Maybe Core.Integer)
deleteDynamicThingGroup_expectedVersion = Lens.lens (\DeleteDynamicThingGroup' {expectedVersion} -> expectedVersion) (\s@DeleteDynamicThingGroup' {} a -> s {expectedVersion = a} :: DeleteDynamicThingGroup)

-- | The name of the dynamic thing group to delete.
deleteDynamicThingGroup_thingGroupName :: Lens.Lens' DeleteDynamicThingGroup Core.Text
deleteDynamicThingGroup_thingGroupName = Lens.lens (\DeleteDynamicThingGroup' {thingGroupName} -> thingGroupName) (\s@DeleteDynamicThingGroup' {} a -> s {thingGroupName = a} :: DeleteDynamicThingGroup)

instance Core.AWSRequest DeleteDynamicThingGroup where
  type
    AWSResponse DeleteDynamicThingGroup =
      DeleteDynamicThingGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDynamicThingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDynamicThingGroup

instance Core.NFData DeleteDynamicThingGroup

instance Core.ToHeaders DeleteDynamicThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDynamicThingGroup where
  toPath DeleteDynamicThingGroup' {..} =
    Core.mconcat
      ["/dynamic-thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery DeleteDynamicThingGroup where
  toQuery DeleteDynamicThingGroup' {..} =
    Core.mconcat
      ["expectedVersion" Core.=: expectedVersion]

-- | /See:/ 'newDeleteDynamicThingGroupResponse' smart constructor.
data DeleteDynamicThingGroupResponse = DeleteDynamicThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDynamicThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDynamicThingGroupResponse_httpStatus' - The response's http status code.
newDeleteDynamicThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDynamicThingGroupResponse
newDeleteDynamicThingGroupResponse pHttpStatus_ =
  DeleteDynamicThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDynamicThingGroupResponse_httpStatus :: Lens.Lens' DeleteDynamicThingGroupResponse Core.Int
deleteDynamicThingGroupResponse_httpStatus = Lens.lens (\DeleteDynamicThingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteDynamicThingGroupResponse' {} a -> s {httpStatus = a} :: DeleteDynamicThingGroupResponse)

instance Core.NFData DeleteDynamicThingGroupResponse
