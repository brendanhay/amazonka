{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType where

import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authentication result.
--
-- /See:/ 'newAuthenticationResultType' smart constructor.
data AuthenticationResultType = AuthenticationResultType'
  { -- | The expiration period of the authentication result in seconds.
    expiresIn :: Core.Maybe Core.Int,
    -- | The token type.
    tokenType :: Core.Maybe Core.Text,
    -- | The access token.
    accessToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The new device metadata from an authentication result.
    newDeviceMetadata' :: Core.Maybe NewDeviceMetadataType,
    -- | The ID token.
    idToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The refresh token.
    refreshToken :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthenticationResultType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresIn', 'authenticationResultType_expiresIn' - The expiration period of the authentication result in seconds.
--
-- 'tokenType', 'authenticationResultType_tokenType' - The token type.
--
-- 'accessToken', 'authenticationResultType_accessToken' - The access token.
--
-- 'newDeviceMetadata'', 'authenticationResultType_newDeviceMetadata' - The new device metadata from an authentication result.
--
-- 'idToken', 'authenticationResultType_idToken' - The ID token.
--
-- 'refreshToken', 'authenticationResultType_refreshToken' - The refresh token.
newAuthenticationResultType ::
  AuthenticationResultType
newAuthenticationResultType =
  AuthenticationResultType'
    { expiresIn = Core.Nothing,
      tokenType = Core.Nothing,
      accessToken = Core.Nothing,
      newDeviceMetadata' = Core.Nothing,
      idToken = Core.Nothing,
      refreshToken = Core.Nothing
    }

-- | The expiration period of the authentication result in seconds.
authenticationResultType_expiresIn :: Lens.Lens' AuthenticationResultType (Core.Maybe Core.Int)
authenticationResultType_expiresIn = Lens.lens (\AuthenticationResultType' {expiresIn} -> expiresIn) (\s@AuthenticationResultType' {} a -> s {expiresIn = a} :: AuthenticationResultType)

-- | The token type.
authenticationResultType_tokenType :: Lens.Lens' AuthenticationResultType (Core.Maybe Core.Text)
authenticationResultType_tokenType = Lens.lens (\AuthenticationResultType' {tokenType} -> tokenType) (\s@AuthenticationResultType' {} a -> s {tokenType = a} :: AuthenticationResultType)

-- | The access token.
authenticationResultType_accessToken :: Lens.Lens' AuthenticationResultType (Core.Maybe Core.Text)
authenticationResultType_accessToken = Lens.lens (\AuthenticationResultType' {accessToken} -> accessToken) (\s@AuthenticationResultType' {} a -> s {accessToken = a} :: AuthenticationResultType) Core.. Lens.mapping Core._Sensitive

-- | The new device metadata from an authentication result.
authenticationResultType_newDeviceMetadata :: Lens.Lens' AuthenticationResultType (Core.Maybe NewDeviceMetadataType)
authenticationResultType_newDeviceMetadata = Lens.lens (\AuthenticationResultType' {newDeviceMetadata'} -> newDeviceMetadata') (\s@AuthenticationResultType' {} a -> s {newDeviceMetadata' = a} :: AuthenticationResultType)

-- | The ID token.
authenticationResultType_idToken :: Lens.Lens' AuthenticationResultType (Core.Maybe Core.Text)
authenticationResultType_idToken = Lens.lens (\AuthenticationResultType' {idToken} -> idToken) (\s@AuthenticationResultType' {} a -> s {idToken = a} :: AuthenticationResultType) Core.. Lens.mapping Core._Sensitive

-- | The refresh token.
authenticationResultType_refreshToken :: Lens.Lens' AuthenticationResultType (Core.Maybe Core.Text)
authenticationResultType_refreshToken = Lens.lens (\AuthenticationResultType' {refreshToken} -> refreshToken) (\s@AuthenticationResultType' {} a -> s {refreshToken = a} :: AuthenticationResultType) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON AuthenticationResultType where
  parseJSON =
    Core.withObject
      "AuthenticationResultType"
      ( \x ->
          AuthenticationResultType'
            Core.<$> (x Core..:? "ExpiresIn")
            Core.<*> (x Core..:? "TokenType")
            Core.<*> (x Core..:? "AccessToken")
            Core.<*> (x Core..:? "NewDeviceMetadata")
            Core.<*> (x Core..:? "IdToken")
            Core.<*> (x Core..:? "RefreshToken")
      )

instance Core.Hashable AuthenticationResultType

instance Core.NFData AuthenticationResultType
