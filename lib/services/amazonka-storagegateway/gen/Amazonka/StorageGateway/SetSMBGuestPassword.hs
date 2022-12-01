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
-- Module      : Amazonka.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@. The @smbguest@ user is
-- the user when the authentication method for the file share is set to
-- @GuestAccess@. This operation only supported for S3 File Gateways
module Amazonka.StorageGateway.SetSMBGuestPassword
  ( -- * Creating a Request
    SetSMBGuestPassword (..),
    newSetSMBGuestPassword,

    -- * Request Lenses
    setSMBGuestPassword_gatewayARN,
    setSMBGuestPassword_password,

    -- * Destructuring the Response
    SetSMBGuestPasswordResponse (..),
    newSetSMBGuestPasswordResponse,

    -- * Response Lenses
    setSMBGuestPasswordResponse_gatewayARN,
    setSMBGuestPasswordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'newSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { -- | The Amazon Resource Name (ARN) of the S3 File Gateway the SMB file share
    -- is associated with.
    gatewayARN :: Prelude.Text,
    -- | The password that you want to set for your SMB server.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSMBGuestPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'setSMBGuestPassword_gatewayARN' - The Amazon Resource Name (ARN) of the S3 File Gateway the SMB file share
-- is associated with.
--
-- 'password', 'setSMBGuestPassword_password' - The password that you want to set for your SMB server.
newSetSMBGuestPassword ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  SetSMBGuestPassword
newSetSMBGuestPassword pGatewayARN_ pPassword_ =
  SetSMBGuestPassword'
    { gatewayARN = pGatewayARN_,
      password = Core._Sensitive Lens.# pPassword_
    }

-- | The Amazon Resource Name (ARN) of the S3 File Gateway the SMB file share
-- is associated with.
setSMBGuestPassword_gatewayARN :: Lens.Lens' SetSMBGuestPassword Prelude.Text
setSMBGuestPassword_gatewayARN = Lens.lens (\SetSMBGuestPassword' {gatewayARN} -> gatewayARN) (\s@SetSMBGuestPassword' {} a -> s {gatewayARN = a} :: SetSMBGuestPassword)

-- | The password that you want to set for your SMB server.
setSMBGuestPassword_password :: Lens.Lens' SetSMBGuestPassword Prelude.Text
setSMBGuestPassword_password = Lens.lens (\SetSMBGuestPassword' {password} -> password) (\s@SetSMBGuestPassword' {} a -> s {password = a} :: SetSMBGuestPassword) Prelude.. Core._Sensitive

instance Core.AWSRequest SetSMBGuestPassword where
  type
    AWSResponse SetSMBGuestPassword =
      SetSMBGuestPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetSMBGuestPasswordResponse'
            Prelude.<$> (x Core..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSMBGuestPassword where
  hashWithSalt _salt SetSMBGuestPassword' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` password

instance Prelude.NFData SetSMBGuestPassword where
  rnf SetSMBGuestPassword' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf password

instance Core.ToHeaders SetSMBGuestPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.SetSMBGuestPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetSMBGuestPassword where
  toJSON SetSMBGuestPassword' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath SetSMBGuestPassword where
  toPath = Prelude.const "/"

instance Core.ToQuery SetSMBGuestPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSMBGuestPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'setSMBGuestPasswordResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'setSMBGuestPasswordResponse_httpStatus' - The response's http status code.
newSetSMBGuestPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetSMBGuestPasswordResponse
newSetSMBGuestPasswordResponse pHttpStatus_ =
  SetSMBGuestPasswordResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
setSMBGuestPasswordResponse_gatewayARN :: Lens.Lens' SetSMBGuestPasswordResponse (Prelude.Maybe Prelude.Text)
setSMBGuestPasswordResponse_gatewayARN = Lens.lens (\SetSMBGuestPasswordResponse' {gatewayARN} -> gatewayARN) (\s@SetSMBGuestPasswordResponse' {} a -> s {gatewayARN = a} :: SetSMBGuestPasswordResponse)

-- | The response's http status code.
setSMBGuestPasswordResponse_httpStatus :: Lens.Lens' SetSMBGuestPasswordResponse Prelude.Int
setSMBGuestPasswordResponse_httpStatus = Lens.lens (\SetSMBGuestPasswordResponse' {httpStatus} -> httpStatus) (\s@SetSMBGuestPasswordResponse' {} a -> s {httpStatus = a} :: SetSMBGuestPasswordResponse)

instance Prelude.NFData SetSMBGuestPasswordResponse where
  rnf SetSMBGuestPasswordResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
