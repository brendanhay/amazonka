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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | SetLocalConsolePasswordInput
--
-- /See:/ 'newSetLocalConsolePassword' smart constructor.
data SetLocalConsolePassword = SetLocalConsolePassword'
  { gatewayARN :: Core.Text,
    -- | The password you want to set for your VM local console.
    localConsolePassword :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'localConsolePassword'
  Core.Text ->
  SetLocalConsolePassword
newSetLocalConsolePassword
  pGatewayARN_
  pLocalConsolePassword_ =
    SetLocalConsolePassword'
      { gatewayARN = pGatewayARN_,
        localConsolePassword =
          Core._Sensitive Lens.# pLocalConsolePassword_
      }

-- | Undocumented member.
setLocalConsolePassword_gatewayARN :: Lens.Lens' SetLocalConsolePassword Core.Text
setLocalConsolePassword_gatewayARN = Lens.lens (\SetLocalConsolePassword' {gatewayARN} -> gatewayARN) (\s@SetLocalConsolePassword' {} a -> s {gatewayARN = a} :: SetLocalConsolePassword)

-- | The password you want to set for your VM local console.
setLocalConsolePassword_localConsolePassword :: Lens.Lens' SetLocalConsolePassword Core.Text
setLocalConsolePassword_localConsolePassword = Lens.lens (\SetLocalConsolePassword' {localConsolePassword} -> localConsolePassword) (\s@SetLocalConsolePassword' {} a -> s {localConsolePassword = a} :: SetLocalConsolePassword) Core.. Core._Sensitive

instance Core.AWSRequest SetLocalConsolePassword where
  type
    AWSResponse SetLocalConsolePassword =
      SetLocalConsolePasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetLocalConsolePasswordResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetLocalConsolePassword

instance Core.NFData SetLocalConsolePassword

instance Core.ToHeaders SetLocalConsolePassword where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.SetLocalConsolePassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetLocalConsolePassword where
  toJSON SetLocalConsolePassword' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just
              ( "LocalConsolePassword"
                  Core..= localConsolePassword
              )
          ]
      )

instance Core.ToPath SetLocalConsolePassword where
  toPath = Core.const "/"

instance Core.ToQuery SetLocalConsolePassword where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetLocalConsolePasswordResponse' smart constructor.
data SetLocalConsolePasswordResponse = SetLocalConsolePasswordResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SetLocalConsolePasswordResponse
newSetLocalConsolePasswordResponse pHttpStatus_ =
  SetLocalConsolePasswordResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
setLocalConsolePasswordResponse_gatewayARN :: Lens.Lens' SetLocalConsolePasswordResponse (Core.Maybe Core.Text)
setLocalConsolePasswordResponse_gatewayARN = Lens.lens (\SetLocalConsolePasswordResponse' {gatewayARN} -> gatewayARN) (\s@SetLocalConsolePasswordResponse' {} a -> s {gatewayARN = a} :: SetLocalConsolePasswordResponse)

-- | The response's http status code.
setLocalConsolePasswordResponse_httpStatus :: Lens.Lens' SetLocalConsolePasswordResponse Core.Int
setLocalConsolePasswordResponse_httpStatus = Lens.lens (\SetLocalConsolePasswordResponse' {httpStatus} -> httpStatus) (\s@SetLocalConsolePasswordResponse' {} a -> s {httpStatus = a} :: SetLocalConsolePasswordResponse)

instance Core.NFData SetLocalConsolePasswordResponse
