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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { -- | The expected version of the thing group to delete.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the thing group to delete.
    thingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteThingGroup
newDeleteThingGroup pThingGroupName_ =
  DeleteThingGroup'
    { expectedVersion =
        Prelude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the thing group to delete.
deleteThingGroup_expectedVersion :: Lens.Lens' DeleteThingGroup (Prelude.Maybe Prelude.Integer)
deleteThingGroup_expectedVersion = Lens.lens (\DeleteThingGroup' {expectedVersion} -> expectedVersion) (\s@DeleteThingGroup' {} a -> s {expectedVersion = a} :: DeleteThingGroup)

-- | The name of the thing group to delete.
deleteThingGroup_thingGroupName :: Lens.Lens' DeleteThingGroup Prelude.Text
deleteThingGroup_thingGroupName = Lens.lens (\DeleteThingGroup' {thingGroupName} -> thingGroupName) (\s@DeleteThingGroup' {} a -> s {thingGroupName = a} :: DeleteThingGroup)

instance Prelude.AWSRequest DeleteThingGroup where
  type Rs DeleteThingGroup = DeleteThingGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteThingGroup

instance Prelude.NFData DeleteThingGroup

instance Prelude.ToHeaders DeleteThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteThingGroup where
  toPath DeleteThingGroup' {..} =
    Prelude.mconcat
      ["/thing-groups/", Prelude.toBS thingGroupName]

instance Prelude.ToQuery DeleteThingGroup where
  toQuery DeleteThingGroup' {..} =
    Prelude.mconcat
      ["expectedVersion" Prelude.=: expectedVersion]

-- | /See:/ 'newDeleteThingGroupResponse' smart constructor.
data DeleteThingGroupResponse = DeleteThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteThingGroupResponse
newDeleteThingGroupResponse pHttpStatus_ =
  DeleteThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteThingGroupResponse_httpStatus :: Lens.Lens' DeleteThingGroupResponse Prelude.Int
deleteThingGroupResponse_httpStatus = Lens.lens (\DeleteThingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteThingGroupResponse' {} a -> s {httpStatus = a} :: DeleteThingGroupResponse)

instance Prelude.NFData DeleteThingGroupResponse
