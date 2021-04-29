{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The authentication result.
--
-- /See:/ 'newAuthenticationResultType' smart constructor.
data AuthenticationResultType = AuthenticationResultType'
  { -- | The expiration period of the authentication result in seconds.
    expiresIn :: Prelude.Maybe Prelude.Int,
    -- | The token type.
    tokenType :: Prelude.Maybe Prelude.Text,
    -- | The access token.
    accessToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The new device metadata from an authentication result.
    newDeviceMetadata' :: Prelude.Maybe NewDeviceMetadataType,
    -- | The ID token.
    idToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The refresh token.
    refreshToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { expiresIn =
        Prelude.Nothing,
      tokenType = Prelude.Nothing,
      accessToken = Prelude.Nothing,
      newDeviceMetadata' = Prelude.Nothing,
      idToken = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | The expiration period of the authentication result in seconds.
authenticationResultType_expiresIn :: Lens.Lens' AuthenticationResultType (Prelude.Maybe Prelude.Int)
authenticationResultType_expiresIn = Lens.lens (\AuthenticationResultType' {expiresIn} -> expiresIn) (\s@AuthenticationResultType' {} a -> s {expiresIn = a} :: AuthenticationResultType)

-- | The token type.
authenticationResultType_tokenType :: Lens.Lens' AuthenticationResultType (Prelude.Maybe Prelude.Text)
authenticationResultType_tokenType = Lens.lens (\AuthenticationResultType' {tokenType} -> tokenType) (\s@AuthenticationResultType' {} a -> s {tokenType = a} :: AuthenticationResultType)

-- | The access token.
authenticationResultType_accessToken :: Lens.Lens' AuthenticationResultType (Prelude.Maybe Prelude.Text)
authenticationResultType_accessToken = Lens.lens (\AuthenticationResultType' {accessToken} -> accessToken) (\s@AuthenticationResultType' {} a -> s {accessToken = a} :: AuthenticationResultType) Prelude.. Lens.mapping Prelude._Sensitive

-- | The new device metadata from an authentication result.
authenticationResultType_newDeviceMetadata :: Lens.Lens' AuthenticationResultType (Prelude.Maybe NewDeviceMetadataType)
authenticationResultType_newDeviceMetadata = Lens.lens (\AuthenticationResultType' {newDeviceMetadata'} -> newDeviceMetadata') (\s@AuthenticationResultType' {} a -> s {newDeviceMetadata' = a} :: AuthenticationResultType)

-- | The ID token.
authenticationResultType_idToken :: Lens.Lens' AuthenticationResultType (Prelude.Maybe Prelude.Text)
authenticationResultType_idToken = Lens.lens (\AuthenticationResultType' {idToken} -> idToken) (\s@AuthenticationResultType' {} a -> s {idToken = a} :: AuthenticationResultType) Prelude.. Lens.mapping Prelude._Sensitive

-- | The refresh token.
authenticationResultType_refreshToken :: Lens.Lens' AuthenticationResultType (Prelude.Maybe Prelude.Text)
authenticationResultType_refreshToken = Lens.lens (\AuthenticationResultType' {refreshToken} -> refreshToken) (\s@AuthenticationResultType' {} a -> s {refreshToken = a} :: AuthenticationResultType) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON AuthenticationResultType where
  parseJSON =
    Prelude.withObject
      "AuthenticationResultType"
      ( \x ->
          AuthenticationResultType'
            Prelude.<$> (x Prelude..:? "ExpiresIn")
            Prelude.<*> (x Prelude..:? "TokenType")
            Prelude.<*> (x Prelude..:? "AccessToken")
            Prelude.<*> (x Prelude..:? "NewDeviceMetadata")
            Prelude.<*> (x Prelude..:? "IdToken")
            Prelude.<*> (x Prelude..:? "RefreshToken")
      )

instance Prelude.Hashable AuthenticationResultType

instance Prelude.NFData AuthenticationResultType
