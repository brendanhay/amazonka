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
-- Module      : Amazonka.SSMContacts.DeleteRotation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a rotation from the system. If a rotation belongs to more than
-- one on-call schedule, this operation deletes it from all of them.
module Amazonka.SSMContacts.DeleteRotation
  ( -- * Creating a Request
    DeleteRotation (..),
    newDeleteRotation,

    -- * Request Lenses
    deleteRotation_rotationId,

    -- * Destructuring the Response
    DeleteRotationResponse (..),
    newDeleteRotationResponse,

    -- * Response Lenses
    deleteRotationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDeleteRotation' smart constructor.
data DeleteRotation = DeleteRotation'
  { -- | The Amazon Resource Name (ARN) of the on-call rotation to delete.
    rotationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRotation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationId', 'deleteRotation_rotationId' - The Amazon Resource Name (ARN) of the on-call rotation to delete.
newDeleteRotation ::
  -- | 'rotationId'
  Prelude.Text ->
  DeleteRotation
newDeleteRotation pRotationId_ =
  DeleteRotation' {rotationId = pRotationId_}

-- | The Amazon Resource Name (ARN) of the on-call rotation to delete.
deleteRotation_rotationId :: Lens.Lens' DeleteRotation Prelude.Text
deleteRotation_rotationId = Lens.lens (\DeleteRotation' {rotationId} -> rotationId) (\s@DeleteRotation' {} a -> s {rotationId = a} :: DeleteRotation)

instance Core.AWSRequest DeleteRotation where
  type
    AWSResponse DeleteRotation =
      DeleteRotationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRotationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRotation where
  hashWithSalt _salt DeleteRotation' {..} =
    _salt `Prelude.hashWithSalt` rotationId

instance Prelude.NFData DeleteRotation where
  rnf DeleteRotation' {..} = Prelude.rnf rotationId

instance Data.ToHeaders DeleteRotation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.DeleteRotation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRotation where
  toJSON DeleteRotation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RotationId" Data..= rotationId)]
      )

instance Data.ToPath DeleteRotation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRotation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRotationResponse' smart constructor.
data DeleteRotationResponse = DeleteRotationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRotationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRotationResponse_httpStatus' - The response's http status code.
newDeleteRotationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRotationResponse
newDeleteRotationResponse pHttpStatus_ =
  DeleteRotationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRotationResponse_httpStatus :: Lens.Lens' DeleteRotationResponse Prelude.Int
deleteRotationResponse_httpStatus = Lens.lens (\DeleteRotationResponse' {httpStatus} -> httpStatus) (\s@DeleteRotationResponse' {} a -> s {httpStatus = a} :: DeleteRotationResponse)

instance Prelude.NFData DeleteRotationResponse where
  rnf DeleteRotationResponse' {..} =
    Prelude.rnf httpStatus
