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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUICustomization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the UI customization information for a user pool\'s built-in app
-- UI.
--
-- You can specify app UI customization settings for a single client (with
-- a specific @clientId@) or for all clients (by setting the @clientId@ to
-- @ALL@). If you specify @ALL@, the default configuration will be used for
-- every client that has no UI customization set previously. If you specify
-- UI customization settings for a particular client, it will no longer
-- fall back to the @ALL@ configuration.
--
-- To use this API, your user pool must have a domain associated with it.
-- Otherwise, there is no place to host the app\'s pages, and the service
-- will throw an error.
module Network.AWS.CognitoIdentityProvider.SetUICustomization
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetUICustomization' smart constructor.
data SetUICustomization = SetUICustomization'
  { -- | The client ID for the client app.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The uploaded logo image for the UI customization.
    imageFile :: Core.Maybe Core.Base64,
    -- | The CSS values in the UI customization.
    css :: Core.Maybe Core.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  SetUICustomization
newSetUICustomization pUserPoolId_ =
  SetUICustomization'
    { clientId = Core.Nothing,
      imageFile = Core.Nothing,
      css = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The client ID for the client app.
setUICustomization_clientId :: Lens.Lens' SetUICustomization (Core.Maybe Core.Text)
setUICustomization_clientId = Lens.lens (\SetUICustomization' {clientId} -> clientId) (\s@SetUICustomization' {} a -> s {clientId = a} :: SetUICustomization) Core.. Lens.mapping Core._Sensitive

-- | The uploaded logo image for the UI customization.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
setUICustomization_imageFile :: Lens.Lens' SetUICustomization (Core.Maybe Core.ByteString)
setUICustomization_imageFile = Lens.lens (\SetUICustomization' {imageFile} -> imageFile) (\s@SetUICustomization' {} a -> s {imageFile = a} :: SetUICustomization) Core.. Lens.mapping Core._Base64

-- | The CSS values in the UI customization.
setUICustomization_css :: Lens.Lens' SetUICustomization (Core.Maybe Core.Text)
setUICustomization_css = Lens.lens (\SetUICustomization' {css} -> css) (\s@SetUICustomization' {} a -> s {css = a} :: SetUICustomization)

-- | The user pool ID for the user pool.
setUICustomization_userPoolId :: Lens.Lens' SetUICustomization Core.Text
setUICustomization_userPoolId = Lens.lens (\SetUICustomization' {userPoolId} -> userPoolId) (\s@SetUICustomization' {} a -> s {userPoolId = a} :: SetUICustomization)

instance Core.AWSRequest SetUICustomization where
  type
    AWSResponse SetUICustomization =
      SetUICustomizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetUICustomizationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "UICustomization")
      )

instance Core.Hashable SetUICustomization

instance Core.NFData SetUICustomization

instance Core.ToHeaders SetUICustomization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.SetUICustomization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetUICustomization where
  toJSON SetUICustomization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientId" Core..=) Core.<$> clientId,
            ("ImageFile" Core..=) Core.<$> imageFile,
            ("CSS" Core..=) Core.<$> css,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath SetUICustomization where
  toPath = Core.const "/"

instance Core.ToQuery SetUICustomization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetUICustomizationResponse' smart constructor.
data SetUICustomizationResponse = SetUICustomizationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The UI customization information.
    uICustomization :: UICustomizationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
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
setUICustomizationResponse_httpStatus :: Lens.Lens' SetUICustomizationResponse Core.Int
setUICustomizationResponse_httpStatus = Lens.lens (\SetUICustomizationResponse' {httpStatus} -> httpStatus) (\s@SetUICustomizationResponse' {} a -> s {httpStatus = a} :: SetUICustomizationResponse)

-- | The UI customization information.
setUICustomizationResponse_uICustomization :: Lens.Lens' SetUICustomizationResponse UICustomizationType
setUICustomizationResponse_uICustomization = Lens.lens (\SetUICustomizationResponse' {uICustomization} -> uICustomization) (\s@SetUICustomizationResponse' {} a -> s {uICustomization = a} :: SetUICustomizationResponse)

instance Core.NFData SetUICustomizationResponse
