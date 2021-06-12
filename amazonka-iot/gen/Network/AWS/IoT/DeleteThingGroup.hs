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
-- Module      : Network.AWS.IoT.DeleteThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a thing group.
module Network.AWS.IoT.DeleteThingGroup
  ( -- * Creating a Request
    DeleteThingGroup (..),
    newDeleteThingGroup,

    -- * Request Lenses
    deleteThingGroup_expectedVersion,
    deleteThingGroup_thingGroupName,

    -- * Destructuring the Response
    DeleteThingGroupResponse (..),
    newDeleteThingGroupResponse,

    -- * Response Lenses
    deleteThingGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { -- | The expected version of the thing group to delete.
    expectedVersion :: Core.Maybe Core.Integer,
    -- | The name of the thing group to delete.
    thingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteThingGroup_expectedVersion' - The expected version of the thing group to delete.
--
-- 'thingGroupName', 'deleteThingGroup_thingGroupName' - The name of the thing group to delete.
newDeleteThingGroup ::
  -- | 'thingGroupName'
  Core.Text ->
  DeleteThingGroup
newDeleteThingGroup pThingGroupName_ =
  DeleteThingGroup'
    { expectedVersion = Core.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the thing group to delete.
deleteThingGroup_expectedVersion :: Lens.Lens' DeleteThingGroup (Core.Maybe Core.Integer)
deleteThingGroup_expectedVersion = Lens.lens (\DeleteThingGroup' {expectedVersion} -> expectedVersion) (\s@DeleteThingGroup' {} a -> s {expectedVersion = a} :: DeleteThingGroup)

-- | The name of the thing group to delete.
deleteThingGroup_thingGroupName :: Lens.Lens' DeleteThingGroup Core.Text
deleteThingGroup_thingGroupName = Lens.lens (\DeleteThingGroup' {thingGroupName} -> thingGroupName) (\s@DeleteThingGroup' {} a -> s {thingGroupName = a} :: DeleteThingGroup)

instance Core.AWSRequest DeleteThingGroup where
  type
    AWSResponse DeleteThingGroup =
      DeleteThingGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteThingGroup

instance Core.NFData DeleteThingGroup

instance Core.ToHeaders DeleteThingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteThingGroup where
  toPath DeleteThingGroup' {..} =
    Core.mconcat
      ["/thing-groups/", Core.toBS thingGroupName]

instance Core.ToQuery DeleteThingGroup where
  toQuery DeleteThingGroup' {..} =
    Core.mconcat
      ["expectedVersion" Core.=: expectedVersion]

-- | /See:/ 'newDeleteThingGroupResponse' smart constructor.
data DeleteThingGroupResponse = DeleteThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteThingGroupResponse_httpStatus' - The response's http status code.
newDeleteThingGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteThingGroupResponse
newDeleteThingGroupResponse pHttpStatus_ =
  DeleteThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteThingGroupResponse_httpStatus :: Lens.Lens' DeleteThingGroupResponse Core.Int
deleteThingGroupResponse_httpStatus = Lens.lens (\DeleteThingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteThingGroupResponse' {} a -> s {httpStatus = a} :: DeleteThingGroupResponse)

instance Core.NFData DeleteThingGroupResponse
