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
-- Module      : Network.AWS.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@. The @smbguest@ user is
-- the user when the authentication method for the file share is set to
-- @GuestAccess@.
module Network.AWS.StorageGateway.SetSMBGuestPassword
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'newSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { -- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is
    -- associated with.
    gatewayARN :: Core.Text,
    -- | The password that you want to set for your SMB server.
    password :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetSMBGuestPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'setSMBGuestPassword_gatewayARN' - The Amazon Resource Name (ARN) of the file gateway the SMB file share is
-- associated with.
--
-- 'password', 'setSMBGuestPassword_password' - The password that you want to set for your SMB server.
newSetSMBGuestPassword ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  SetSMBGuestPassword
newSetSMBGuestPassword pGatewayARN_ pPassword_ =
  SetSMBGuestPassword'
    { gatewayARN = pGatewayARN_,
      password = Core._Sensitive Lens.# pPassword_
    }

-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is
-- associated with.
setSMBGuestPassword_gatewayARN :: Lens.Lens' SetSMBGuestPassword Core.Text
setSMBGuestPassword_gatewayARN = Lens.lens (\SetSMBGuestPassword' {gatewayARN} -> gatewayARN) (\s@SetSMBGuestPassword' {} a -> s {gatewayARN = a} :: SetSMBGuestPassword)

-- | The password that you want to set for your SMB server.
setSMBGuestPassword_password :: Lens.Lens' SetSMBGuestPassword Core.Text
setSMBGuestPassword_password = Lens.lens (\SetSMBGuestPassword' {password} -> password) (\s@SetSMBGuestPassword' {} a -> s {password = a} :: SetSMBGuestPassword) Core.. Core._Sensitive

instance Core.AWSRequest SetSMBGuestPassword where
  type
    AWSResponse SetSMBGuestPassword =
      SetSMBGuestPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetSMBGuestPasswordResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetSMBGuestPassword

instance Core.NFData SetSMBGuestPassword

instance Core.ToHeaders SetSMBGuestPassword where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.SetSMBGuestPassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetSMBGuestPassword where
  toJSON SetSMBGuestPassword' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath SetSMBGuestPassword where
  toPath = Core.const "/"

instance Core.ToQuery SetSMBGuestPassword where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SetSMBGuestPasswordResponse
newSetSMBGuestPasswordResponse pHttpStatus_ =
  SetSMBGuestPasswordResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
setSMBGuestPasswordResponse_gatewayARN :: Lens.Lens' SetSMBGuestPasswordResponse (Core.Maybe Core.Text)
setSMBGuestPasswordResponse_gatewayARN = Lens.lens (\SetSMBGuestPasswordResponse' {gatewayARN} -> gatewayARN) (\s@SetSMBGuestPasswordResponse' {} a -> s {gatewayARN = a} :: SetSMBGuestPasswordResponse)

-- | The response's http status code.
setSMBGuestPasswordResponse_httpStatus :: Lens.Lens' SetSMBGuestPasswordResponse Core.Int
setSMBGuestPasswordResponse_httpStatus = Lens.lens (\SetSMBGuestPasswordResponse' {httpStatus} -> httpStatus) (\s@SetSMBGuestPasswordResponse' {} a -> s {httpStatus = a} :: SetSMBGuestPasswordResponse)

instance Core.NFData SetSMBGuestPasswordResponse
