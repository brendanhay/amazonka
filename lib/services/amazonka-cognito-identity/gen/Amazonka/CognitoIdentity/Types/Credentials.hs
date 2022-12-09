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
-- Module      : Amazonka.CognitoIdentity.Types.Credentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.Credentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Credentials for the provided identity ID.
--
-- /See:/ 'newCredentials' smart constructor.
data Credentials = Credentials'
  { -- | The Access Key portion of the credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date at which these credentials will expire.
    expiration :: Prelude.Maybe Data.POSIX,
    -- | The Secret Access Key portion of the credentials
    secretKey :: Prelude.Maybe Prelude.Text,
    -- | The Session Token portion of the credentials
    sessionToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Credentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'credentials_accessKeyId' - The Access Key portion of the credentials.
--
-- 'expiration', 'credentials_expiration' - The date at which these credentials will expire.
--
-- 'secretKey', 'credentials_secretKey' - The Secret Access Key portion of the credentials
--
-- 'sessionToken', 'credentials_sessionToken' - The Session Token portion of the credentials
newCredentials ::
  Credentials
newCredentials =
  Credentials'
    { accessKeyId = Prelude.Nothing,
      expiration = Prelude.Nothing,
      secretKey = Prelude.Nothing,
      sessionToken = Prelude.Nothing
    }

-- | The Access Key portion of the credentials.
credentials_accessKeyId :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_accessKeyId = Lens.lens (\Credentials' {accessKeyId} -> accessKeyId) (\s@Credentials' {} a -> s {accessKeyId = a} :: Credentials)

-- | The date at which these credentials will expire.
credentials_expiration :: Lens.Lens' Credentials (Prelude.Maybe Prelude.UTCTime)
credentials_expiration = Lens.lens (\Credentials' {expiration} -> expiration) (\s@Credentials' {} a -> s {expiration = a} :: Credentials) Prelude.. Lens.mapping Data._Time

-- | The Secret Access Key portion of the credentials
credentials_secretKey :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_secretKey = Lens.lens (\Credentials' {secretKey} -> secretKey) (\s@Credentials' {} a -> s {secretKey = a} :: Credentials)

-- | The Session Token portion of the credentials
credentials_sessionToken :: Lens.Lens' Credentials (Prelude.Maybe Prelude.Text)
credentials_sessionToken = Lens.lens (\Credentials' {sessionToken} -> sessionToken) (\s@Credentials' {} a -> s {sessionToken = a} :: Credentials)

instance Data.FromJSON Credentials where
  parseJSON =
    Data.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Prelude.<$> (x Data..:? "AccessKeyId")
            Prelude.<*> (x Data..:? "Expiration")
            Prelude.<*> (x Data..:? "SecretKey")
            Prelude.<*> (x Data..:? "SessionToken")
      )

instance Prelude.Hashable Credentials where
  hashWithSalt _salt Credentials' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` secretKey
      `Prelude.hashWithSalt` sessionToken

instance Prelude.NFData Credentials where
  rnf Credentials' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf secretKey
      `Prelude.seq` Prelude.rnf sessionToken
