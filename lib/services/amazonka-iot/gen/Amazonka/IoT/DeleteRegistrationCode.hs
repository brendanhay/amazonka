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
-- Module      : Amazonka.IoT.DeleteRegistrationCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CA certificate registration code.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteRegistrationCode>
-- action.
module Amazonka.IoT.DeleteRegistrationCode
  ( -- * Creating a Request
    DeleteRegistrationCode (..),
    newDeleteRegistrationCode,

    -- * Destructuring the Response
    DeleteRegistrationCodeResponse (..),
    newDeleteRegistrationCodeResponse,

    -- * Response Lenses
    deleteRegistrationCodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteRegistrationCode operation.
--
-- /See:/ 'newDeleteRegistrationCode' smart constructor.
data DeleteRegistrationCode = DeleteRegistrationCode'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistrationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRegistrationCode ::
  DeleteRegistrationCode
newDeleteRegistrationCode = DeleteRegistrationCode'

instance Core.AWSRequest DeleteRegistrationCode where
  type
    AWSResponse DeleteRegistrationCode =
      DeleteRegistrationCodeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRegistrationCodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRegistrationCode where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteRegistrationCode where
  rnf _ = ()

instance Core.ToHeaders DeleteRegistrationCode where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteRegistrationCode where
  toPath = Prelude.const "/registrationcode"

instance Core.ToQuery DeleteRegistrationCode where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeleteRegistrationCode operation.
--
-- /See:/ 'newDeleteRegistrationCodeResponse' smart constructor.
data DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistrationCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRegistrationCodeResponse_httpStatus' - The response's http status code.
newDeleteRegistrationCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRegistrationCodeResponse
newDeleteRegistrationCodeResponse pHttpStatus_ =
  DeleteRegistrationCodeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRegistrationCodeResponse_httpStatus :: Lens.Lens' DeleteRegistrationCodeResponse Prelude.Int
deleteRegistrationCodeResponse_httpStatus = Lens.lens (\DeleteRegistrationCodeResponse' {httpStatus} -> httpStatus) (\s@DeleteRegistrationCodeResponse' {} a -> s {httpStatus = a} :: DeleteRegistrationCodeResponse)

instance
  Prelude.NFData
    DeleteRegistrationCodeResponse
  where
  rnf DeleteRegistrationCodeResponse' {..} =
    Prelude.rnf httpStatus
