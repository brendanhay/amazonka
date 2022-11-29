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
-- Module      : Amazonka.XRay.DeleteGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group resource.
module Amazonka.XRay.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_groupName,
    deleteGroup_groupARN,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group that was generated on creation.
    groupARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'deleteGroup_groupName' - The case-sensitive name of the group.
--
-- 'groupARN', 'deleteGroup_groupARN' - The ARN of the group that was generated on creation.
newDeleteGroup ::
  DeleteGroup
newDeleteGroup =
  DeleteGroup'
    { groupName = Prelude.Nothing,
      groupARN = Prelude.Nothing
    }

-- | The case-sensitive name of the group.
deleteGroup_groupName :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

-- | The ARN of the group that was generated on creation.
deleteGroup_groupARN :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_groupARN = Lens.lens (\DeleteGroup' {groupARN} -> groupARN) (\s@DeleteGroup' {} a -> s {groupARN = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroup where
  hashWithSalt _salt DeleteGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupARN

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupARN

instance Core.ToHeaders DeleteGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GroupName" Core..=) Prelude.<$> groupName,
            ("GroupARN" Core..=) Prelude.<$> groupARN
          ]
      )

instance Core.ToPath DeleteGroup where
  toPath = Prelude.const "/DeleteGroup"

instance Core.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGroupResponse_httpStatus' - The response's http status code.
newDeleteGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Prelude.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Prelude.NFData DeleteGroupResponse where
  rnf DeleteGroupResponse' {..} = Prelude.rnf httpStatus
