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
-- Module      : Amazonka.CognitoIdentityProvider.SetUICustomization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user interface (UI) customization information for a user
-- pool\'s built-in app UI.
--
-- You can specify app UI customization settings for a single client (with
-- a specific @clientId@) or for all clients (by setting the @clientId@ to
-- @ALL@). If you specify @ALL@, the default configuration is used for
-- every client that has no previously set UI customization. If you specify
-- UI customization settings for a particular client, it will no longer
-- return to the @ALL@ configuration.
--
-- To use this API, your user pool must have a domain associated with it.
-- Otherwise, there is no place to host the app\'s pages, and the service
-- will throw an error.
module Amazonka.CognitoIdentityProvider.SetUICustomization
  ( -- * Creating a Request
    SetUICustomization (..),
    newSetUICustomization,

    -- * Request Lenses
    setUICustomization_clientId,
    setUICustomization_imageFile,
    setUICustomization_css,
    setUICustomization_userPoolId,

    -- * Destructuring the Response
    SetUICustomizationResponse (..),
    newSetUICustomizationResponse,

    -- * Response Lenses
    setUICustomizationResponse_httpStatus,
    setUICustomizationResponse_uICustomization,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetUICustomization' smart constructor.
data SetUICustomization = SetUICustomization'
  { -- | The client ID for the client app.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The uploaded logo image for the UI customization.
    imageFile :: Prelude.Maybe Data.Base64,
    -- | The CSS values in the UI customization.
    css :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUICustomization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'setUICustomization_clientId' - The client ID for the client app.
--
-- 'imageFile', 'setUICustomization_imageFile' - The uploaded logo image for the UI customization.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'css', 'setUICustomization_css' - The CSS values in the UI customization.
--
-- 'userPoolId', 'setUICustomization_userPoolId' - The user pool ID for the user pool.
newSetUICustomization ::
  -- | 'userPoolId'
  Prelude.Text ->
  SetUICustomization
newSetUICustomization pUserPoolId_ =
  SetUICustomization'
    { clientId = Prelude.Nothing,
      imageFile = Prelude.Nothing,
      css = Prelude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The client ID for the client app.
setUICustomization_clientId :: Lens.Lens' SetUICustomization (Prelude.Maybe Prelude.Text)
setUICustomization_clientId = Lens.lens (\SetUICustomization' {clientId} -> clientId) (\s@SetUICustomization' {} a -> s {clientId = a} :: SetUICustomization) Prelude.. Lens.mapping Data._Sensitive

-- | The uploaded logo image for the UI customization.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
setUICustomization_imageFile :: Lens.Lens' SetUICustomization (Prelude.Maybe Prelude.ByteString)
setUICustomization_imageFile = Lens.lens (\SetUICustomization' {imageFile} -> imageFile) (\s@SetUICustomization' {} a -> s {imageFile = a} :: SetUICustomization) Prelude.. Lens.mapping Data._Base64

-- | The CSS values in the UI customization.
setUICustomization_css :: Lens.Lens' SetUICustomization (Prelude.Maybe Prelude.Text)
setUICustomization_css = Lens.lens (\SetUICustomization' {css} -> css) (\s@SetUICustomization' {} a -> s {css = a} :: SetUICustomization)

-- | The user pool ID for the user pool.
setUICustomization_userPoolId :: Lens.Lens' SetUICustomization Prelude.Text
setUICustomization_userPoolId = Lens.lens (\SetUICustomization' {userPoolId} -> userPoolId) (\s@SetUICustomization' {} a -> s {userPoolId = a} :: SetUICustomization)

instance Core.AWSRequest SetUICustomization where
  type
    AWSResponse SetUICustomization =
      SetUICustomizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetUICustomizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UICustomization")
      )

instance Prelude.Hashable SetUICustomization where
  hashWithSalt _salt SetUICustomization' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` imageFile
      `Prelude.hashWithSalt` css
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData SetUICustomization where
  rnf SetUICustomization' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf imageFile
      `Prelude.seq` Prelude.rnf css
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders SetUICustomization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.SetUICustomization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetUICustomization where
  toJSON SetUICustomization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientId" Data..=) Prelude.<$> clientId,
            ("ImageFile" Data..=) Prelude.<$> imageFile,
            ("CSS" Data..=) Prelude.<$> css,
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath SetUICustomization where
  toPath = Prelude.const "/"

instance Data.ToQuery SetUICustomization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetUICustomizationResponse' smart constructor.
data SetUICustomizationResponse = SetUICustomizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The UI customization information.
    uICustomization :: UICustomizationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetUICustomizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setUICustomizationResponse_httpStatus' - The response's http status code.
--
-- 'uICustomization', 'setUICustomizationResponse_uICustomization' - The UI customization information.
newSetUICustomizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uICustomization'
  UICustomizationType ->
  SetUICustomizationResponse
newSetUICustomizationResponse
  pHttpStatus_
  pUICustomization_ =
    SetUICustomizationResponse'
      { httpStatus =
          pHttpStatus_,
        uICustomization = pUICustomization_
      }

-- | The response's http status code.
setUICustomizationResponse_httpStatus :: Lens.Lens' SetUICustomizationResponse Prelude.Int
setUICustomizationResponse_httpStatus = Lens.lens (\SetUICustomizationResponse' {httpStatus} -> httpStatus) (\s@SetUICustomizationResponse' {} a -> s {httpStatus = a} :: SetUICustomizationResponse)

-- | The UI customization information.
setUICustomizationResponse_uICustomization :: Lens.Lens' SetUICustomizationResponse UICustomizationType
setUICustomizationResponse_uICustomization = Lens.lens (\SetUICustomizationResponse' {uICustomization} -> uICustomization) (\s@SetUICustomizationResponse' {} a -> s {uICustomization = a} :: SetUICustomizationResponse)

instance Prelude.NFData SetUICustomizationResponse where
  rnf SetUICustomizationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uICustomization
