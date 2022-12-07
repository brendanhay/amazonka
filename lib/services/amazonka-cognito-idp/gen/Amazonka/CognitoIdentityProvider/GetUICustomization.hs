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
-- Module      : Amazonka.CognitoIdentityProvider.GetUICustomization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user interface (UI) Customization information for a particular
-- app client\'s app UI, if any such information exists for the client. If
-- nothing is set for the particular client, but there is an existing pool
-- level customization (the app @clientId@ is @ALL@), then that information
-- is returned. If nothing is present, then an empty shape is returned.
module Amazonka.CognitoIdentityProvider.GetUICustomization
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUICustomization' smart constructor.
data GetUICustomization = GetUICustomization'
  { -- | The client ID for the client app.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
getUICustomization_clientId = Lens.lens (\GetUICustomization' {clientId} -> clientId) (\s@GetUICustomization' {} a -> s {clientId = a} :: GetUICustomization) Prelude.. Lens.mapping Data._Sensitive

-- | The user pool ID for the user pool.
getUICustomization_userPoolId :: Lens.Lens' GetUICustomization Prelude.Text
getUICustomization_userPoolId = Lens.lens (\GetUICustomization' {userPoolId} -> userPoolId) (\s@GetUICustomization' {} a -> s {userPoolId = a} :: GetUICustomization)

instance Core.AWSRequest GetUICustomization where
  type
    AWSResponse GetUICustomization =
      GetUICustomizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUICustomizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UICustomization")
      )

instance Prelude.Hashable GetUICustomization where
  hashWithSalt _salt GetUICustomization' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData GetUICustomization where
  rnf GetUICustomization' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders GetUICustomization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.GetUICustomization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUICustomization where
  toJSON GetUICustomization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientId" Data..=) Prelude.<$> clientId,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath GetUICustomization where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUICustomization where
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

instance Prelude.NFData GetUICustomizationResponse where
  rnf GetUICustomizationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uICustomization
