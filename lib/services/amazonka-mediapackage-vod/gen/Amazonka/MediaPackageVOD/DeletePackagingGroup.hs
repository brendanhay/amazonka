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
-- Module      : Amazonka.MediaPackageVOD.DeletePackagingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a MediaPackage VOD PackagingGroup resource.
module Amazonka.MediaPackageVOD.DeletePackagingGroup
  ( -- * Creating a Request
    DeletePackagingGroup (..),
    newDeletePackagingGroup,

    -- * Request Lenses
    deletePackagingGroup_id,

    -- * Destructuring the Response
    DeletePackagingGroupResponse (..),
    newDeletePackagingGroupResponse,

    -- * Response Lenses
    deletePackagingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackagingGroup' smart constructor.
data DeletePackagingGroup = DeletePackagingGroup'
  { -- | The ID of the MediaPackage VOD PackagingGroup resource to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackagingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deletePackagingGroup_id' - The ID of the MediaPackage VOD PackagingGroup resource to delete.
newDeletePackagingGroup ::
  -- | 'id'
  Prelude.Text ->
  DeletePackagingGroup
newDeletePackagingGroup pId_ =
  DeletePackagingGroup' {id = pId_}

-- | The ID of the MediaPackage VOD PackagingGroup resource to delete.
deletePackagingGroup_id :: Lens.Lens' DeletePackagingGroup Prelude.Text
deletePackagingGroup_id = Lens.lens (\DeletePackagingGroup' {id} -> id) (\s@DeletePackagingGroup' {} a -> s {id = a} :: DeletePackagingGroup)

instance Core.AWSRequest DeletePackagingGroup where
  type
    AWSResponse DeletePackagingGroup =
      DeletePackagingGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePackagingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackagingGroup where
  hashWithSalt _salt DeletePackagingGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeletePackagingGroup where
  rnf DeletePackagingGroup' {..} = Prelude.rnf id

instance Data.ToHeaders DeletePackagingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePackagingGroup where
  toPath DeletePackagingGroup' {..} =
    Prelude.mconcat
      ["/packaging_groups/", Data.toBS id]

instance Data.ToQuery DeletePackagingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePackagingGroupResponse' smart constructor.
data DeletePackagingGroupResponse = DeletePackagingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackagingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePackagingGroupResponse_httpStatus' - The response's http status code.
newDeletePackagingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackagingGroupResponse
newDeletePackagingGroupResponse pHttpStatus_ =
  DeletePackagingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePackagingGroupResponse_httpStatus :: Lens.Lens' DeletePackagingGroupResponse Prelude.Int
deletePackagingGroupResponse_httpStatus = Lens.lens (\DeletePackagingGroupResponse' {httpStatus} -> httpStatus) (\s@DeletePackagingGroupResponse' {} a -> s {httpStatus = a} :: DeletePackagingGroupResponse)

instance Prelude.NFData DeletePackagingGroupResponse where
  rnf DeletePackagingGroupResponse' {..} =
    Prelude.rnf httpStatus
