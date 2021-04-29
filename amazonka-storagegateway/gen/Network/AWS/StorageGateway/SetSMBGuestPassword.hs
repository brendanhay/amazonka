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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'newSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { -- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is
    -- associated with.
    gatewayARN :: Prelude.Text,
    -- | The password that you want to set for your SMB server.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  SetSMBGuestPassword
newSetSMBGuestPassword pGatewayARN_ pPassword_ =
  SetSMBGuestPassword'
    { gatewayARN = pGatewayARN_,
      password = Prelude._Sensitive Lens.# pPassword_
    }

-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is
-- associated with.
setSMBGuestPassword_gatewayARN :: Lens.Lens' SetSMBGuestPassword Prelude.Text
setSMBGuestPassword_gatewayARN = Lens.lens (\SetSMBGuestPassword' {gatewayARN} -> gatewayARN) (\s@SetSMBGuestPassword' {} a -> s {gatewayARN = a} :: SetSMBGuestPassword)

-- | The password that you want to set for your SMB server.
setSMBGuestPassword_password :: Lens.Lens' SetSMBGuestPassword Prelude.Text
setSMBGuestPassword_password = Lens.lens (\SetSMBGuestPassword' {password} -> password) (\s@SetSMBGuestPassword' {} a -> s {password = a} :: SetSMBGuestPassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest SetSMBGuestPassword where
  type
    Rs SetSMBGuestPassword =
      SetSMBGuestPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetSMBGuestPasswordResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSMBGuestPassword

instance Prelude.NFData SetSMBGuestPassword

instance Prelude.ToHeaders SetSMBGuestPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.SetSMBGuestPassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetSMBGuestPassword where
  toJSON SetSMBGuestPassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath SetSMBGuestPassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetSMBGuestPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData SetSMBGuestPasswordResponse
