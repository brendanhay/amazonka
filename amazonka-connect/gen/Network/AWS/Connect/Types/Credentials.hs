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
-- Module      : Network.AWS.Connect.Types.Credentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Credentials where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains credentials to use for federation.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | Renews the expiration timer for a generated token.
    refreshTokenExpiration :: Prelude.Maybe Prelude.POSIX,
    -- | An access token generated for a federated user to access Amazon Connect.
    accessToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | A token generated with an expiration time for the session a user is
    -- logged in to Amazon Connect.
    accessTokenExpiration :: Prelude.Maybe Prelude.POSIX,
    -- | Renews a token generated for a user to access the Amazon Connect
    -- instance.
    refreshToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { refreshTokenExpiration =
        Prelude.Nothing,
      accessToken = Prelude.Nothing,
      accessTokenExpiration = Prelude.Nothing,
      refreshToken = Prelude.Nothing
    }

-- | Renews the expiration timer for a generated token.
credentials_refreshTokenExpiration :: Lens.Lens' Credentials (Prelude.Maybe Prelude.UTCTime)
credentials_refreshTokenExpiration = Lens.lens (\Credentials' {refreshTokenExpiration} -> refreshTokenExpiration) (\s@Credentials' {} a -> s {refreshTokenExpiration = a} :: Credentials) Prelude.. Lens.mapping Prelude._Time

-- | An access token generated for a federated user to access Amazon Connect.
credentials_accessToken :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_accessToken = Lens.lens (\Credentials' {accessToken} -> accessToken) (\s@Credentials' {} a -> s {accessToken = a} :: Credentials) Prelude.. Lens.mapping Prelude._Sensitive

-- | A token generated with an expiration time for the session a user is
-- logged in to Amazon Connect.
credentials_accessTokenExpiration :: Lens.Lens' Credentials (Prelude.Maybe Prelude.UTCTime)
credentials_accessTokenExpiration = Lens.lens (\Credentials' {accessTokenExpiration} -> accessTokenExpiration) (\s@Credentials' {} a -> s {accessTokenExpiration = a} :: Credentials) Prelude.. Lens.mapping Prelude._Time

-- | Renews a token generated for a user to access the Amazon Connect
-- instance.
credentials_refreshToken :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_refreshToken = Lens.lens (\Credentials' {refreshToken} -> refreshToken) (\s@Credentials' {} a -> s {refreshToken = a} :: Credentials) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON Credentials where
  parseJSON =
    Prelude.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Prelude.<$> (x Prelude..:? "RefreshTokenExpiration")
            Prelude.<*> (x Prelude..:? "AccessToken")
            Prelude.<*> (x Prelude..:? "AccessTokenExpiration")
            Prelude.<*> (x Prelude..:? "RefreshToken")
      )

instance Prelude.Hashable Credentials

instance Prelude.NFData Credentials
