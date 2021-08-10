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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUICustomization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the UI Customization information for a particular app client\'s app
-- UI, if there is something set. If nothing is set for the particular
-- client, but there is an existing pool level customization (app
-- @clientId@ will be @ALL@), then that is returned. If nothing is present,
-- then an empty shape is returned.
module Network.AWS.CognitoIdentityProvider.GetUICustomization
  ( -- * Creating a Request
    GetUICustomization (..),
    newGetUICustomization,

    -- * Request Lenses
    getUICustomization_clientId,
    getUICustomization_userPoolId,

    -- * Destructuring the Response
    GetUICustomizationResponse (..),
    newGetUICustomizationResponse,

    -- * Response Lenses
    getUICustomizationResponse_httpStatus,
    getUICustomizationResponse_uICustomization,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUICustomization' smart constructor.
data GetUICustomization = GetUICustomization'
  { -- | The client ID for the client app.
    clientId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUICustomization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'getUICustomization_clientId' - The client ID for the client app.
--
-- 'userPoolId', 'getUICustomization_userPoolId' - The user pool ID for the user pool.
newGetUICustomization ::
  -- | 'userPoolId'
  Prelude.Text ->
  GetUICustomization
newGetUICustomization pUserPoolId_ =
  GetUICustomization'
    { clientId = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The client ID for the client app.
getUICustomization_clientId :: Lens.Lens' GetUICustomization (Prelude.Maybe Prelude.Text)
getUICustomization_clientId = Lens.lens (\GetUICustomization' {clientId} -> clientId) (\s@GetUICustomization' {} a -> s {clientId = a} :: GetUICustomization) Prelude.. Lens.mapping Core._Sensitive

-- | The user pool ID for the user pool.
getUICustomization_userPoolId :: Lens.Lens' GetUICustomization Prelude.Text
getUICustomization_userPoolId = Lens.lens (\GetUICustomization' {userPoolId} -> userPoolId) (\s@GetUICustomization' {} a -> s {userPoolId = a} :: GetUICustomization)

instance Core.AWSRequest GetUICustomization where
  type
    AWSResponse GetUICustomization =
      GetUICustomizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUICustomizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "UICustomization")
      )

instance Prelude.Hashable GetUICustomization

instance Prelude.NFData GetUICustomization

instance Core.ToHeaders GetUICustomization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUICustomization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUICustomization where
  toJSON GetUICustomization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientId" Core..=) Prelude.<$> clientId,
            Prelude.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath GetUICustomization where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUICustomization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUICustomizationResponse' smart constructor.
data GetUICustomizationResponse = GetUICustomizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The UI customization information.
    uICustomization :: UICustomizationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUICustomizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getUICustomizationResponse_httpStatus' - The response's http status code.
--
-- 'uICustomization', 'getUICustomizationResponse_uICustomization' - The UI customization information.
newGetUICustomizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uICustomization'
  UICustomizationType ->
  GetUICustomizationResponse
newGetUICustomizationResponse
  pHttpStatus_
  pUICustomization_ =
    GetUICustomizationResponse'
      { httpStatus =
          pHttpStatus_,
        uICustomization = pUICustomization_
      }

-- | The response's http status code.
getUICustomizationResponse_httpStatus :: Lens.Lens' GetUICustomizationResponse Prelude.Int
getUICustomizationResponse_httpStatus = Lens.lens (\GetUICustomizationResponse' {httpStatus} -> httpStatus) (\s@GetUICustomizationResponse' {} a -> s {httpStatus = a} :: GetUICustomizationResponse)

-- | The UI customization information.
getUICustomizationResponse_uICustomization :: Lens.Lens' GetUICustomizationResponse UICustomizationType
getUICustomizationResponse_uICustomization = Lens.lens (\GetUICustomizationResponse' {uICustomization} -> uICustomization) (\s@GetUICustomizationResponse' {} a -> s {uICustomization = a} :: GetUICustomizationResponse)

instance Prelude.NFData GetUICustomizationResponse
