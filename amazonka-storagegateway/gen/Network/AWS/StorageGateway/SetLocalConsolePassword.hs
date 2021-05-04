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
-- Module      : Network.AWS.StorageGateway.SetLocalConsolePassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for your VM local console. When you log in to the
-- local console for the first time, you log in to the VM with the default
-- credentials. We recommend that you set a new password. You don\'t need
-- to know the default password to set a new password.
module Network.AWS.StorageGateway.SetLocalConsolePassword
  ( -- * Creating a Request
    SetLocalConsolePassword (..),
    newSetLocalConsolePassword,

    -- * Request Lenses
    setLocalConsolePassword_gatewayARN,
    setLocalConsolePassword_localConsolePassword,

    -- * Destructuring the Response
    SetLocalConsolePasswordResponse (..),
    newSetLocalConsolePasswordResponse,

    -- * Response Lenses
    setLocalConsolePasswordResponse_gatewayARN,
    setLocalConsolePasswordResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | SetLocalConsolePasswordInput
--
-- /See:/ 'newSetLocalConsolePassword' smart constructor.
data SetLocalConsolePassword = SetLocalConsolePassword'
  { gatewayARN :: Prelude.Text,
    -- | The password you want to set for your VM local console.
    localConsolePassword :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLocalConsolePassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'setLocalConsolePassword_gatewayARN' - Undocumented member.
--
-- 'localConsolePassword', 'setLocalConsolePassword_localConsolePassword' - The password you want to set for your VM local console.
newSetLocalConsolePassword ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'localConsolePassword'
  Prelude.Text ->
  SetLocalConsolePassword
newSetLocalConsolePassword
  pGatewayARN_
  pLocalConsolePassword_ =
    SetLocalConsolePassword'
      { gatewayARN = pGatewayARN_,
        localConsolePassword =
          Prelude._Sensitive Lens.# pLocalConsolePassword_
      }

-- | Undocumented member.
setLocalConsolePassword_gatewayARN :: Lens.Lens' SetLocalConsolePassword Prelude.Text
setLocalConsolePassword_gatewayARN = Lens.lens (\SetLocalConsolePassword' {gatewayARN} -> gatewayARN) (\s@SetLocalConsolePassword' {} a -> s {gatewayARN = a} :: SetLocalConsolePassword)

-- | The password you want to set for your VM local console.
setLocalConsolePassword_localConsolePassword :: Lens.Lens' SetLocalConsolePassword Prelude.Text
setLocalConsolePassword_localConsolePassword = Lens.lens (\SetLocalConsolePassword' {localConsolePassword} -> localConsolePassword) (\s@SetLocalConsolePassword' {} a -> s {localConsolePassword = a} :: SetLocalConsolePassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest SetLocalConsolePassword where
  type
    Rs SetLocalConsolePassword =
      SetLocalConsolePasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetLocalConsolePasswordResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetLocalConsolePassword

instance Prelude.NFData SetLocalConsolePassword

instance Prelude.ToHeaders SetLocalConsolePassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.SetLocalConsolePassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetLocalConsolePassword where
  toJSON SetLocalConsolePassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just
              ( "LocalConsolePassword"
                  Prelude..= localConsolePassword
              )
          ]
      )

instance Prelude.ToPath SetLocalConsolePassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetLocalConsolePassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetLocalConsolePasswordResponse' smart constructor.
data SetLocalConsolePasswordResponse = SetLocalConsolePasswordResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLocalConsolePasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'setLocalConsolePasswordResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'setLocalConsolePasswordResponse_httpStatus' - The response's http status code.
newSetLocalConsolePasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetLocalConsolePasswordResponse
newSetLocalConsolePasswordResponse pHttpStatus_ =
  SetLocalConsolePasswordResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
setLocalConsolePasswordResponse_gatewayARN :: Lens.Lens' SetLocalConsolePasswordResponse (Prelude.Maybe Prelude.Text)
setLocalConsolePasswordResponse_gatewayARN = Lens.lens (\SetLocalConsolePasswordResponse' {gatewayARN} -> gatewayARN) (\s@SetLocalConsolePasswordResponse' {} a -> s {gatewayARN = a} :: SetLocalConsolePasswordResponse)

-- | The response's http status code.
setLocalConsolePasswordResponse_httpStatus :: Lens.Lens' SetLocalConsolePasswordResponse Prelude.Int
setLocalConsolePasswordResponse_httpStatus = Lens.lens (\SetLocalConsolePasswordResponse' {httpStatus} -> httpStatus) (\s@SetLocalConsolePasswordResponse' {} a -> s {httpStatus = a} :: SetLocalConsolePasswordResponse)

instance
  Prelude.NFData
    SetLocalConsolePasswordResponse
