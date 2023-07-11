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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group resource.
module Amazonka.XRay.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_groupARN,
    deleteGroup_groupName,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The ARN of the group that was generated on creation.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text
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
-- 'groupARN', 'deleteGroup_groupARN' - The ARN of the group that was generated on creation.
--
-- 'groupName', 'deleteGroup_groupName' - The case-sensitive name of the group.
newDeleteGroup ::
  DeleteGroup
newDeleteGroup =
  DeleteGroup'
    { groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The ARN of the group that was generated on creation.
deleteGroup_groupARN :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_groupARN = Lens.lens (\DeleteGroup' {groupARN} -> groupARN) (\s@DeleteGroup' {} a -> s {groupARN = a} :: DeleteGroup)

-- | The case-sensitive name of the group.
deleteGroup_groupName :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

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
    _salt
      `Prelude.hashWithSalt` groupARN
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} =
    Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders DeleteGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupARN" Data..=) Prelude.<$> groupARN,
            ("GroupName" Data..=) Prelude.<$> groupName
          ]
      )

instance Data.ToPath DeleteGroup where
  toPath = Prelude.const "/DeleteGroup"

instance Data.ToQuery DeleteGroup where
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
