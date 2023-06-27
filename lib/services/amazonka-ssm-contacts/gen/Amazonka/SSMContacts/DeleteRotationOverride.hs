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
-- Module      : Amazonka.SSMContacts.DeleteRotationOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing override for an on-call rotation.
module Amazonka.SSMContacts.DeleteRotationOverride
  ( -- * Creating a Request
    DeleteRotationOverride (..),
    newDeleteRotationOverride,

    -- * Request Lenses
    deleteRotationOverride_rotationId,
    deleteRotationOverride_rotationOverrideId,

    -- * Destructuring the Response
    DeleteRotationOverrideResponse (..),
    newDeleteRotationOverrideResponse,

    -- * Response Lenses
    deleteRotationOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDeleteRotationOverride' smart constructor.
data DeleteRotationOverride = DeleteRotationOverride'
  { -- | The Amazon Resource Name (ARN) of the rotation that was overridden.
    rotationId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the on-call rotation override to
    -- delete.
    rotationOverrideId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRotationOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationId', 'deleteRotationOverride_rotationId' - The Amazon Resource Name (ARN) of the rotation that was overridden.
--
-- 'rotationOverrideId', 'deleteRotationOverride_rotationOverrideId' - The Amazon Resource Name (ARN) of the on-call rotation override to
-- delete.
newDeleteRotationOverride ::
  -- | 'rotationId'
  Prelude.Text ->
  -- | 'rotationOverrideId'
  Prelude.Text ->
  DeleteRotationOverride
newDeleteRotationOverride
  pRotationId_
  pRotationOverrideId_ =
    DeleteRotationOverride'
      { rotationId = pRotationId_,
        rotationOverrideId = pRotationOverrideId_
      }

-- | The Amazon Resource Name (ARN) of the rotation that was overridden.
deleteRotationOverride_rotationId :: Lens.Lens' DeleteRotationOverride Prelude.Text
deleteRotationOverride_rotationId = Lens.lens (\DeleteRotationOverride' {rotationId} -> rotationId) (\s@DeleteRotationOverride' {} a -> s {rotationId = a} :: DeleteRotationOverride)

-- | The Amazon Resource Name (ARN) of the on-call rotation override to
-- delete.
deleteRotationOverride_rotationOverrideId :: Lens.Lens' DeleteRotationOverride Prelude.Text
deleteRotationOverride_rotationOverrideId = Lens.lens (\DeleteRotationOverride' {rotationOverrideId} -> rotationOverrideId) (\s@DeleteRotationOverride' {} a -> s {rotationOverrideId = a} :: DeleteRotationOverride)

instance Core.AWSRequest DeleteRotationOverride where
  type
    AWSResponse DeleteRotationOverride =
      DeleteRotationOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRotationOverrideResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRotationOverride where
  hashWithSalt _salt DeleteRotationOverride' {..} =
    _salt
      `Prelude.hashWithSalt` rotationId
      `Prelude.hashWithSalt` rotationOverrideId

instance Prelude.NFData DeleteRotationOverride where
  rnf DeleteRotationOverride' {..} =
    Prelude.rnf rotationId
      `Prelude.seq` Prelude.rnf rotationOverrideId

instance Data.ToHeaders DeleteRotationOverride where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.DeleteRotationOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRotationOverride where
  toJSON DeleteRotationOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RotationId" Data..= rotationId),
            Prelude.Just
              ("RotationOverrideId" Data..= rotationOverrideId)
          ]
      )

instance Data.ToPath DeleteRotationOverride where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRotationOverride where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRotationOverrideResponse' smart constructor.
data DeleteRotationOverrideResponse = DeleteRotationOverrideResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRotationOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRotationOverrideResponse_httpStatus' - The response's http status code.
newDeleteRotationOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRotationOverrideResponse
newDeleteRotationOverrideResponse pHttpStatus_ =
  DeleteRotationOverrideResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRotationOverrideResponse_httpStatus :: Lens.Lens' DeleteRotationOverrideResponse Prelude.Int
deleteRotationOverrideResponse_httpStatus = Lens.lens (\DeleteRotationOverrideResponse' {httpStatus} -> httpStatus) (\s@DeleteRotationOverrideResponse' {} a -> s {httpStatus = a} :: DeleteRotationOverrideResponse)

instance
  Prelude.NFData
    DeleteRotationOverrideResponse
  where
  rnf DeleteRotationOverrideResponse' {..} =
    Prelude.rnf httpStatus
