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
-- Module      : Amazonka.SSM.DeleteActivation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activation. You aren\'t required to delete an activation. If
-- you delete an activation, you can no longer use it to register
-- additional managed nodes. Deleting an activation doesn\'t de-register
-- managed nodes. You must manually de-register managed nodes.
module Amazonka.SSM.DeleteActivation
  ( -- * Creating a Request
    DeleteActivation (..),
    newDeleteActivation,

    -- * Request Lenses
    deleteActivation_activationId,

    -- * Destructuring the Response
    DeleteActivationResponse (..),
    newDeleteActivationResponse,

    -- * Response Lenses
    deleteActivationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteActivation' smart constructor.
data DeleteActivation = DeleteActivation'
  { -- | The ID of the activation that you want to delete.
    activationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteActivation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationId', 'deleteActivation_activationId' - The ID of the activation that you want to delete.
newDeleteActivation ::
  -- | 'activationId'
  Prelude.Text ->
  DeleteActivation
newDeleteActivation pActivationId_ =
  DeleteActivation' {activationId = pActivationId_}

-- | The ID of the activation that you want to delete.
deleteActivation_activationId :: Lens.Lens' DeleteActivation Prelude.Text
deleteActivation_activationId = Lens.lens (\DeleteActivation' {activationId} -> activationId) (\s@DeleteActivation' {} a -> s {activationId = a} :: DeleteActivation)

instance Core.AWSRequest DeleteActivation where
  type
    AWSResponse DeleteActivation =
      DeleteActivationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteActivation where
  hashWithSalt _salt DeleteActivation' {..} =
    _salt `Prelude.hashWithSalt` activationId

instance Prelude.NFData DeleteActivation where
  rnf DeleteActivation' {..} = Prelude.rnf activationId

instance Data.ToHeaders DeleteActivation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DeleteActivation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteActivation where
  toJSON DeleteActivation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ActivationId" Data..= activationId)]
      )

instance Data.ToPath DeleteActivation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteActivation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActivationResponse' smart constructor.
data DeleteActivationResponse = DeleteActivationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteActivationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteActivationResponse_httpStatus' - The response's http status code.
newDeleteActivationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteActivationResponse
newDeleteActivationResponse pHttpStatus_ =
  DeleteActivationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteActivationResponse_httpStatus :: Lens.Lens' DeleteActivationResponse Prelude.Int
deleteActivationResponse_httpStatus = Lens.lens (\DeleteActivationResponse' {httpStatus} -> httpStatus) (\s@DeleteActivationResponse' {} a -> s {httpStatus = a} :: DeleteActivationResponse)

instance Prelude.NFData DeleteActivationResponse where
  rnf DeleteActivationResponse' {..} =
    Prelude.rnf httpStatus
