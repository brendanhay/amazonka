{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DeleteRegistrationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CA certificate registration code.
module Network.AWS.IoT.DeleteRegistrationCode
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteRegistrationCode operation.
--
-- /See:/ 'newDeleteRegistrationCode' smart constructor.
data DeleteRegistrationCode = DeleteRegistrationCode'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistrationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRegistrationCode ::
  DeleteRegistrationCode
newDeleteRegistrationCode = DeleteRegistrationCode'

instance Prelude.AWSRequest DeleteRegistrationCode where
  type
    Rs DeleteRegistrationCode =
      DeleteRegistrationCodeResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRegistrationCodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRegistrationCode

instance Prelude.NFData DeleteRegistrationCode

instance Prelude.ToHeaders DeleteRegistrationCode where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRegistrationCode where
  toPath = Prelude.const "/registrationcode"

instance Prelude.ToQuery DeleteRegistrationCode where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeleteRegistrationCode operation.
--
-- /See:/ 'newDeleteRegistrationCodeResponse' smart constructor.
data DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
