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
-- Module      : Amazonka.Shield.DeleteProtectionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified protection group.
module Amazonka.Shield.DeleteProtectionGroup
  ( -- * Creating a Request
    DeleteProtectionGroup (..),
    newDeleteProtectionGroup,

    -- * Request Lenses
    deleteProtectionGroup_protectionGroupId,

    -- * Destructuring the Response
    DeleteProtectionGroupResponse (..),
    newDeleteProtectionGroupResponse,

    -- * Response Lenses
    deleteProtectionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDeleteProtectionGroup' smart constructor.
data DeleteProtectionGroup = DeleteProtectionGroup'
  { -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionGroupId', 'deleteProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
newDeleteProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  DeleteProtectionGroup
newDeleteProtectionGroup pProtectionGroupId_ =
  DeleteProtectionGroup'
    { protectionGroupId =
        pProtectionGroupId_
    }

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
deleteProtectionGroup_protectionGroupId :: Lens.Lens' DeleteProtectionGroup Prelude.Text
deleteProtectionGroup_protectionGroupId = Lens.lens (\DeleteProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@DeleteProtectionGroup' {} a -> s {protectionGroupId = a} :: DeleteProtectionGroup)

instance Core.AWSRequest DeleteProtectionGroup where
  type
    AWSResponse DeleteProtectionGroup =
      DeleteProtectionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProtectionGroup where
  hashWithSalt _salt DeleteProtectionGroup' {..} =
    _salt `Prelude.hashWithSalt` protectionGroupId

instance Prelude.NFData DeleteProtectionGroup where
  rnf DeleteProtectionGroup' {..} =
    Prelude.rnf protectionGroupId

instance Data.ToHeaders DeleteProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DeleteProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProtectionGroup where
  toJSON DeleteProtectionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionGroupId" Data..= protectionGroupId)
          ]
      )

instance Data.ToPath DeleteProtectionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProtectionGroupResponse' smart constructor.
data DeleteProtectionGroupResponse = DeleteProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProtectionGroupResponse_httpStatus' - The response's http status code.
newDeleteProtectionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProtectionGroupResponse
newDeleteProtectionGroupResponse pHttpStatus_ =
  DeleteProtectionGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProtectionGroupResponse_httpStatus :: Lens.Lens' DeleteProtectionGroupResponse Prelude.Int
deleteProtectionGroupResponse_httpStatus = Lens.lens (\DeleteProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteProtectionGroupResponse' {} a -> s {httpStatus = a} :: DeleteProtectionGroupResponse)

instance Prelude.NFData DeleteProtectionGroupResponse where
  rnf DeleteProtectionGroupResponse' {..} =
    Prelude.rnf httpStatus
