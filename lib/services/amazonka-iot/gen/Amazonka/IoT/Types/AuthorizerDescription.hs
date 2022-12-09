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
-- Module      : Amazonka.IoT.Types.AuthorizerDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthorizerDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AuthorizerStatus
import qualified Amazonka.Prelude as Prelude

-- | The authorizer description.
--
-- /See:/ 'newAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer\'s Lambda function ARN.
    authorizerFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the authorizer was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | When @true@, the result from the authorizer’s Lambda function is cached
    -- for the time specified in @refreshAfterInSeconds@. The cached result is
    -- used while the device reuses the same HTTP connection.
    enableCachingForHttp :: Prelude.Maybe Prelude.Bool,
    -- | The UNIX timestamp of when the authorizer was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether IoT validates the token signature in an authorization
    -- request.
    signingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The status of the authorizer.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | The public keys used to validate the token signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerArn', 'authorizerDescription_authorizerArn' - The authorizer ARN.
--
-- 'authorizerFunctionArn', 'authorizerDescription_authorizerFunctionArn' - The authorizer\'s Lambda function ARN.
--
-- 'authorizerName', 'authorizerDescription_authorizerName' - The authorizer name.
--
-- 'creationDate', 'authorizerDescription_creationDate' - The UNIX timestamp of when the authorizer was created.
--
-- 'enableCachingForHttp', 'authorizerDescription_enableCachingForHttp' - When @true@, the result from the authorizer’s Lambda function is cached
-- for the time specified in @refreshAfterInSeconds@. The cached result is
-- used while the device reuses the same HTTP connection.
--
-- 'lastModifiedDate', 'authorizerDescription_lastModifiedDate' - The UNIX timestamp of when the authorizer was last updated.
--
-- 'signingDisabled', 'authorizerDescription_signingDisabled' - Specifies whether IoT validates the token signature in an authorization
-- request.
--
-- 'status', 'authorizerDescription_status' - The status of the authorizer.
--
-- 'tokenKeyName', 'authorizerDescription_tokenKeyName' - The key used to extract the token from the HTTP headers.
--
-- 'tokenSigningPublicKeys', 'authorizerDescription_tokenSigningPublicKeys' - The public keys used to validate the token signature returned by your
-- custom authentication service.
newAuthorizerDescription ::
  AuthorizerDescription
newAuthorizerDescription =
  AuthorizerDescription'
    { authorizerArn =
        Prelude.Nothing,
      authorizerFunctionArn = Prelude.Nothing,
      authorizerName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      enableCachingForHttp = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      signingDisabled = Prelude.Nothing,
      status = Prelude.Nothing,
      tokenKeyName = Prelude.Nothing,
      tokenSigningPublicKeys = Prelude.Nothing
    }

-- | The authorizer ARN.
authorizerDescription_authorizerArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerArn = Lens.lens (\AuthorizerDescription' {authorizerArn} -> authorizerArn) (\s@AuthorizerDescription' {} a -> s {authorizerArn = a} :: AuthorizerDescription)

-- | The authorizer\'s Lambda function ARN.
authorizerDescription_authorizerFunctionArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerFunctionArn = Lens.lens (\AuthorizerDescription' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@AuthorizerDescription' {} a -> s {authorizerFunctionArn = a} :: AuthorizerDescription)

-- | The authorizer name.
authorizerDescription_authorizerName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerName = Lens.lens (\AuthorizerDescription' {authorizerName} -> authorizerName) (\s@AuthorizerDescription' {} a -> s {authorizerName = a} :: AuthorizerDescription)

-- | The UNIX timestamp of when the authorizer was created.
authorizerDescription_creationDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_creationDate = Lens.lens (\AuthorizerDescription' {creationDate} -> creationDate) (\s@AuthorizerDescription' {} a -> s {creationDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Data._Time

-- | When @true@, the result from the authorizer’s Lambda function is cached
-- for the time specified in @refreshAfterInSeconds@. The cached result is
-- used while the device reuses the same HTTP connection.
authorizerDescription_enableCachingForHttp :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Bool)
authorizerDescription_enableCachingForHttp = Lens.lens (\AuthorizerDescription' {enableCachingForHttp} -> enableCachingForHttp) (\s@AuthorizerDescription' {} a -> s {enableCachingForHttp = a} :: AuthorizerDescription)

-- | The UNIX timestamp of when the authorizer was last updated.
authorizerDescription_lastModifiedDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_lastModifiedDate = Lens.lens (\AuthorizerDescription' {lastModifiedDate} -> lastModifiedDate) (\s@AuthorizerDescription' {} a -> s {lastModifiedDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Data._Time

-- | Specifies whether IoT validates the token signature in an authorization
-- request.
authorizerDescription_signingDisabled :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Bool)
authorizerDescription_signingDisabled = Lens.lens (\AuthorizerDescription' {signingDisabled} -> signingDisabled) (\s@AuthorizerDescription' {} a -> s {signingDisabled = a} :: AuthorizerDescription)

-- | The status of the authorizer.
authorizerDescription_status :: Lens.Lens' AuthorizerDescription (Prelude.Maybe AuthorizerStatus)
authorizerDescription_status = Lens.lens (\AuthorizerDescription' {status} -> status) (\s@AuthorizerDescription' {} a -> s {status = a} :: AuthorizerDescription)

-- | The key used to extract the token from the HTTP headers.
authorizerDescription_tokenKeyName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_tokenKeyName = Lens.lens (\AuthorizerDescription' {tokenKeyName} -> tokenKeyName) (\s@AuthorizerDescription' {} a -> s {tokenKeyName = a} :: AuthorizerDescription)

-- | The public keys used to validate the token signature returned by your
-- custom authentication service.
authorizerDescription_tokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authorizerDescription_tokenSigningPublicKeys = Lens.lens (\AuthorizerDescription' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@AuthorizerDescription' {} a -> s {tokenSigningPublicKeys = a} :: AuthorizerDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AuthorizerDescription where
  parseJSON =
    Data.withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            Prelude.<$> (x Data..:? "authorizerArn")
            Prelude.<*> (x Data..:? "authorizerFunctionArn")
            Prelude.<*> (x Data..:? "authorizerName")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "enableCachingForHttp")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "signingDisabled")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tokenKeyName")
            Prelude.<*> ( x Data..:? "tokenSigningPublicKeys"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AuthorizerDescription where
  hashWithSalt _salt AuthorizerDescription' {..} =
    _salt `Prelude.hashWithSalt` authorizerArn
      `Prelude.hashWithSalt` authorizerFunctionArn
      `Prelude.hashWithSalt` authorizerName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` enableCachingForHttp
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` signingDisabled
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tokenKeyName
      `Prelude.hashWithSalt` tokenSigningPublicKeys

instance Prelude.NFData AuthorizerDescription where
  rnf AuthorizerDescription' {..} =
    Prelude.rnf authorizerArn
      `Prelude.seq` Prelude.rnf authorizerFunctionArn
      `Prelude.seq` Prelude.rnf authorizerName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf enableCachingForHttp
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf signingDisabled
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tokenKeyName
      `Prelude.seq` Prelude.rnf tokenSigningPublicKeys
