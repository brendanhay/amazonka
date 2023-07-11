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
-- Module      : Amazonka.IoTWireless.DeleteMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a multicast group if it is not in use by a fuota task.
module Amazonka.IoTWireless.DeleteMulticastGroup
  ( -- * Creating a Request
    DeleteMulticastGroup (..),
    newDeleteMulticastGroup,

    -- * Request Lenses
    deleteMulticastGroup_id,

    -- * Destructuring the Response
    DeleteMulticastGroupResponse (..),
    newDeleteMulticastGroupResponse,

    -- * Response Lenses
    deleteMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMulticastGroup' smart constructor.
data DeleteMulticastGroup = DeleteMulticastGroup'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteMulticastGroup_id' - Undocumented member.
newDeleteMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  DeleteMulticastGroup
newDeleteMulticastGroup pId_ =
  DeleteMulticastGroup' {id = pId_}

-- | Undocumented member.
deleteMulticastGroup_id :: Lens.Lens' DeleteMulticastGroup Prelude.Text
deleteMulticastGroup_id = Lens.lens (\DeleteMulticastGroup' {id} -> id) (\s@DeleteMulticastGroup' {} a -> s {id = a} :: DeleteMulticastGroup)

instance Core.AWSRequest DeleteMulticastGroup where
  type
    AWSResponse DeleteMulticastGroup =
      DeleteMulticastGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMulticastGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMulticastGroup where
  hashWithSalt _salt DeleteMulticastGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteMulticastGroup where
  rnf DeleteMulticastGroup' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteMulticastGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteMulticastGroup where
  toPath DeleteMulticastGroup' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id]

instance Data.ToQuery DeleteMulticastGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMulticastGroupResponse' smart constructor.
data DeleteMulticastGroupResponse = DeleteMulticastGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMulticastGroupResponse_httpStatus' - The response's http status code.
newDeleteMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMulticastGroupResponse
newDeleteMulticastGroupResponse pHttpStatus_ =
  DeleteMulticastGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMulticastGroupResponse_httpStatus :: Lens.Lens' DeleteMulticastGroupResponse Prelude.Int
deleteMulticastGroupResponse_httpStatus = Lens.lens (\DeleteMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteMulticastGroupResponse' {} a -> s {httpStatus = a} :: DeleteMulticastGroupResponse)

instance Prelude.NFData DeleteMulticastGroupResponse where
  rnf DeleteMulticastGroupResponse' {..} =
    Prelude.rnf httpStatus
