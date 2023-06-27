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
-- Module      : Amazonka.IoT.DeleteDynamicThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dynamic thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteDynamicThingGroup>
-- action.
module Amazonka.IoT.DeleteDynamicThingGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDynamicThingGroup' smart constructor.
data DeleteDynamicThingGroup = DeleteDynamicThingGroup'
  { -- | The expected version of the dynamic thing group to delete.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the dynamic thing group to delete.
    thingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDynamicThingGroup
newDeleteDynamicThingGroup pThingGroupName_ =
  DeleteDynamicThingGroup'
    { expectedVersion =
        Prelude.Nothing,
      thingGroupName = pThingGroupName_
    }

-- | The expected version of the dynamic thing group to delete.
deleteDynamicThingGroup_expectedVersion :: Lens.Lens' DeleteDynamicThingGroup (Prelude.Maybe Prelude.Integer)
deleteDynamicThingGroup_expectedVersion = Lens.lens (\DeleteDynamicThingGroup' {expectedVersion} -> expectedVersion) (\s@DeleteDynamicThingGroup' {} a -> s {expectedVersion = a} :: DeleteDynamicThingGroup)

-- | The name of the dynamic thing group to delete.
deleteDynamicThingGroup_thingGroupName :: Lens.Lens' DeleteDynamicThingGroup Prelude.Text
deleteDynamicThingGroup_thingGroupName = Lens.lens (\DeleteDynamicThingGroup' {thingGroupName} -> thingGroupName) (\s@DeleteDynamicThingGroup' {} a -> s {thingGroupName = a} :: DeleteDynamicThingGroup)

instance Core.AWSRequest DeleteDynamicThingGroup where
  type
    AWSResponse DeleteDynamicThingGroup =
      DeleteDynamicThingGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDynamicThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDynamicThingGroup where
  hashWithSalt _salt DeleteDynamicThingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` thingGroupName

instance Prelude.NFData DeleteDynamicThingGroup where
  rnf DeleteDynamicThingGroup' {..} =
    Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf thingGroupName

instance Data.ToHeaders DeleteDynamicThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDynamicThingGroup where
  toPath DeleteDynamicThingGroup' {..} =
    Prelude.mconcat
      ["/dynamic-thing-groups/", Data.toBS thingGroupName]

instance Data.ToQuery DeleteDynamicThingGroup where
  toQuery DeleteDynamicThingGroup' {..} =
    Prelude.mconcat
      ["expectedVersion" Data.=: expectedVersion]

-- | /See:/ 'newDeleteDynamicThingGroupResponse' smart constructor.
data DeleteDynamicThingGroupResponse = DeleteDynamicThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDynamicThingGroupResponse
newDeleteDynamicThingGroupResponse pHttpStatus_ =
  DeleteDynamicThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDynamicThingGroupResponse_httpStatus :: Lens.Lens' DeleteDynamicThingGroupResponse Prelude.Int
deleteDynamicThingGroupResponse_httpStatus = Lens.lens (\DeleteDynamicThingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteDynamicThingGroupResponse' {} a -> s {httpStatus = a} :: DeleteDynamicThingGroupResponse)

instance
  Prelude.NFData
    DeleteDynamicThingGroupResponse
  where
  rnf DeleteDynamicThingGroupResponse' {..} =
    Prelude.rnf httpStatus
