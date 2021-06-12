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
-- Module      : Network.AWS.Connect.Types.Credentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Credentials where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains credentials to use for federation.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | Renews the expiration timer for a generated token.
    refreshTokenExpiration :: Core.Maybe Core.POSIX,
    -- | An access token generated for a federated user to access Amazon Connect.
    accessToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A token generated with an expiration time for the session a user is
    -- logged in to Amazon Connect.
    accessTokenExpiration :: Core.Maybe Core.POSIX,
    -- | Renews a token generated for a user to access the Amazon Connect
    -- instance.
    refreshToken :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshTokenExpiration', 'credentials_refreshTokenExpiration' - Renews the expiration timer for a generated token.
--
-- 'accessToken', 'credentials_accessToken' - An access token generated for a federated user to access Amazon Connect.
--
-- 'accessTokenExpiration', 'credentials_accessTokenExpiration' - A token generated with an expiration time for the session a user is
-- logged in to Amazon Connect.
--
-- 'refreshToken', 'credentials_refreshToken' - Renews a token generated for a user to access the Amazon Connect
-- instance.
newCredentials ::
  Credentials
newCredentials =
  Credentials'
    { refreshTokenExpiration = Core.Nothing,
      accessToken = Core.Nothing,
      accessTokenExpiration = Core.Nothing,
      refreshToken = Core.Nothing
    }

-- | Renews the expiration timer for a generated token.
credentials_refreshTokenExpiration :: Lens.Lens' Credentials (Core.Maybe Core.UTCTime)
credentials_refreshTokenExpiration = Lens.lens (\Credentials' {refreshTokenExpiration} -> refreshTokenExpiration) (\s@Credentials' {} a -> s {refreshTokenExpiration = a} :: Credentials) Core.. Lens.mapping Core._Time

-- | An access token generated for a federated user to access Amazon Connect.
credentials_accessToken :: Lens.Lens' Credentials (Core.Maybe Core.Text)
credentials_accessToken = Lens.lens (\Credentials' {accessToken} -> accessToken) (\s@Credentials' {} a -> s {accessToken = a} :: Credentials) Core.. Lens.mapping Core._Sensitive

-- | A token generated with an expiration time for the session a user is
-- logged in to Amazon Connect.
credentials_accessTokenExpiration :: Lens.Lens' Credentials (Core.Maybe Core.UTCTime)
credentials_accessTokenExpiration = Lens.lens (\Credentials' {accessTokenExpiration} -> accessTokenExpiration) (\s@Credentials' {} a -> s {accessTokenExpiration = a} :: Credentials) Core.. Lens.mapping Core._Time

-- | Renews a token generated for a user to access the Amazon Connect
-- instance.
credentials_refreshToken :: Lens.Lens' Credentials (Core.Maybe Core.Text)
credentials_refreshToken = Lens.lens (\Credentials' {refreshToken} -> refreshToken) (\s@Credentials' {} a -> s {refreshToken = a} :: Credentials) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON Credentials where
  parseJSON =
    Core.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Core.<$> (x Core..:? "RefreshTokenExpiration")
            Core.<*> (x Core..:? "AccessToken")
            Core.<*> (x Core..:? "AccessTokenExpiration")
            Core.<*> (x Core..:? "RefreshToken")
      )

instance Core.Hashable Credentials

instance Core.NFData Credentials
