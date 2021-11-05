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
-- Module      : Amazonka.CognitoIdentityProvider.GlobalSignOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices. It also invalidates all refresh tokens
-- issued to a user. The user\'s current access and Id tokens remain valid
-- until their expiry. Access and Id tokens expire one hour after they are
-- issued.
module Amazonka.CognitoIdentityProvider.GlobalSignOut
  ( -- * Creating a Request
    GlobalSignOut (..),
    newGlobalSignOut,

    -- * Request Lenses
    globalSignOut_accessToken,

    -- * Destructuring the Response
    GlobalSignOutResponse (..),
    newGlobalSignOutResponse,

    -- * Response Lenses
    globalSignOutResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to sign out all devices.
--
-- /See:/ 'newGlobalSignOut' smart constructor.
data GlobalSignOut = GlobalSignOut'
  { -- | The access token.
    accessToken :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSignOut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'globalSignOut_accessToken' - The access token.
newGlobalSignOut ::
  -- | 'accessToken'
  Prelude.Text ->
  GlobalSignOut
newGlobalSignOut pAccessToken_ =
  GlobalSignOut'
    { accessToken =
        Core._Sensitive Lens.# pAccessToken_
    }

-- | The access token.
globalSignOut_accessToken :: Lens.Lens' GlobalSignOut Prelude.Text
globalSignOut_accessToken = Lens.lens (\GlobalSignOut' {accessToken} -> accessToken) (\s@GlobalSignOut' {} a -> s {accessToken = a} :: GlobalSignOut) Prelude.. Core._Sensitive

instance Core.AWSRequest GlobalSignOut where
  type
    AWSResponse GlobalSignOut =
      GlobalSignOutResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          GlobalSignOutResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GlobalSignOut

instance Prelude.NFData GlobalSignOut

instance Core.ToHeaders GlobalSignOut where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GlobalSignOut" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GlobalSignOut where
  toJSON GlobalSignOut' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AccessToken" Core..= accessToken)]
      )

instance Core.ToPath GlobalSignOut where
  toPath = Prelude.const "/"

instance Core.ToQuery GlobalSignOut where
  toQuery = Prelude.const Prelude.mempty

-- | The response to the request to sign out all devices.
--
-- /See:/ 'newGlobalSignOutResponse' smart constructor.
data GlobalSignOutResponse = GlobalSignOutResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSignOutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'globalSignOutResponse_httpStatus' - The response's http status code.
newGlobalSignOutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GlobalSignOutResponse
newGlobalSignOutResponse pHttpStatus_ =
  GlobalSignOutResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
globalSignOutResponse_httpStatus :: Lens.Lens' GlobalSignOutResponse Prelude.Int
globalSignOutResponse_httpStatus = Lens.lens (\GlobalSignOutResponse' {httpStatus} -> httpStatus) (\s@GlobalSignOutResponse' {} a -> s {httpStatus = a} :: GlobalSignOutResponse)

instance Prelude.NFData GlobalSignOutResponse
