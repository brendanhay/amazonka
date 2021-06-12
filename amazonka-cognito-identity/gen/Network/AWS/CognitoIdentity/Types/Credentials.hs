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
-- Module      : Network.AWS.CognitoIdentity.Types.Credentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.Credentials where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Credentials for the provided identity ID.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | The date at which these credentials will expire.
    expiration :: Core.Maybe Core.POSIX,
    -- | The Secret Access Key portion of the credentials
    secretKey :: Core.Maybe Core.Text,
    -- | The Access Key portion of the credentials.
    accessKeyId :: Core.Maybe Core.Text,
    -- | The Session Token portion of the credentials
    sessionToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'credentials_expiration' - The date at which these credentials will expire.
--
-- 'secretKey', 'credentials_secretKey' - The Secret Access Key portion of the credentials
--
-- 'accessKeyId', 'credentials_accessKeyId' - The Access Key portion of the credentials.
--
-- 'sessionToken', 'credentials_sessionToken' - The Session Token portion of the credentials
newCredentials ::
  Credentials
newCredentials =
  Credentials'
    { expiration = Core.Nothing,
      secretKey = Core.Nothing,
      accessKeyId = Core.Nothing,
      sessionToken = Core.Nothing
    }

-- | The date at which these credentials will expire.
credentials_expiration :: Lens.Lens' Credentials (Core.Maybe Core.UTCTime)
credentials_expiration = Lens.lens (\Credentials' {expiration} -> expiration) (\s@Credentials' {} a -> s {expiration = a} :: Credentials) Core.. Lens.mapping Core._Time

-- | The Secret Access Key portion of the credentials
credentials_secretKey :: Lens.Lens' Credentials (Core.Maybe Core.Text)
credentials_secretKey = Lens.lens (\Credentials' {secretKey} -> secretKey) (\s@Credentials' {} a -> s {secretKey = a} :: Credentials)

-- | The Access Key portion of the credentials.
credentials_accessKeyId :: Lens.Lens' Credentials (Core.Maybe Core.Text)
credentials_accessKeyId = Lens.lens (\Credentials' {accessKeyId} -> accessKeyId) (\s@Credentials' {} a -> s {accessKeyId = a} :: Credentials)

-- | The Session Token portion of the credentials
credentials_sessionToken :: Lens.Lens' Credentials (Core.Maybe Core.Text)
credentials_sessionToken = Lens.lens (\Credentials' {sessionToken} -> sessionToken) (\s@Credentials' {} a -> s {sessionToken = a} :: Credentials)

instance Core.FromJSON Credentials where
  parseJSON =
    Core.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Core.<$> (x Core..:? "Expiration")
            Core.<*> (x Core..:? "SecretKey")
            Core.<*> (x Core..:? "AccessKeyId")
            Core.<*> (x Core..:? "SessionToken")
      )

instance Core.Hashable Credentials

instance Core.NFData Credentials
