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
import Amazonka.IoT.Types.AuthorizerStatus
import qualified Amazonka.Prelude as Prelude

-- | The authorizer description.
--
-- /See:/ 'newAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { -- | The UNIX timestamp of when the authorizer was last updated.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the authorizer was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The status of the authorizer.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The authorizer\'s Lambda function ARN.
    authorizerFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether IoT validates the token signature in an authorization
    -- request.
    signingDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The public keys used to validate the token signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | When @true@, the result from the authorizer’s Lambda function is cached
    -- for the time specified in @refreshAfterInSeconds@. The cached result is
    -- used while the device reuses the same HTTP connection.
    enableCachingForHttp :: Prelude.Maybe Prelude.Bool,
    -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text
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
-- 'lastModifiedDate', 'authorizerDescription_lastModifiedDate' - The UNIX timestamp of when the authorizer was last updated.
--
-- 'tokenKeyName', 'authorizerDescription_tokenKeyName' - The key used to extract the token from the HTTP headers.
--
-- 'creationDate', 'authorizerDescription_creationDate' - The UNIX timestamp of when the authorizer was created.
--
-- 'status', 'authorizerDescription_status' - The status of the authorizer.
--
-- 'authorizerFunctionArn', 'authorizerDescription_authorizerFunctionArn' - The authorizer\'s Lambda function ARN.
--
-- 'signingDisabled', 'authorizerDescription_signingDisabled' - Specifies whether IoT validates the token signature in an authorization
-- request.
--
-- 'tokenSigningPublicKeys', 'authorizerDescription_tokenSigningPublicKeys' - The public keys used to validate the token signature returned by your
-- custom authentication service.
--
-- 'authorizerArn', 'authorizerDescription_authorizerArn' - The authorizer ARN.
--
-- 'enableCachingForHttp', 'authorizerDescription_enableCachingForHttp' - When @true@, the result from the authorizer’s Lambda function is cached
-- for the time specified in @refreshAfterInSeconds@. The cached result is
-- used while the device reuses the same HTTP connection.
--
-- 'authorizerName', 'authorizerDescription_authorizerName' - The authorizer name.
newAuthorizerDescription ::
  AuthorizerDescription
newAuthorizerDescription =
  AuthorizerDescription'
    { lastModifiedDate =
        Prelude.Nothing,
      tokenKeyName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      authorizerFunctionArn = Prelude.Nothing,
      signingDisabled = Prelude.Nothing,
      tokenSigningPublicKeys = Prelude.Nothing,
      authorizerArn = Prelude.Nothing,
      enableCachingForHttp = Prelude.Nothing,
      authorizerName = Prelude.Nothing
    }

-- | The UNIX timestamp of when the authorizer was last updated.
authorizerDescription_lastModifiedDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_lastModifiedDate = Lens.lens (\AuthorizerDescription' {lastModifiedDate} -> lastModifiedDate) (\s@AuthorizerDescription' {} a -> s {lastModifiedDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Core._Time

-- | The key used to extract the token from the HTTP headers.
authorizerDescription_tokenKeyName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_tokenKeyName = Lens.lens (\AuthorizerDescription' {tokenKeyName} -> tokenKeyName) (\s@AuthorizerDescription' {} a -> s {tokenKeyName = a} :: AuthorizerDescription)

-- | The UNIX timestamp of when the authorizer was created.
authorizerDescription_creationDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_creationDate = Lens.lens (\AuthorizerDescription' {creationDate} -> creationDate) (\s@AuthorizerDescription' {} a -> s {creationDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Core._Time

-- | The status of the authorizer.
authorizerDescription_status :: Lens.Lens' AuthorizerDescription (Prelude.Maybe AuthorizerStatus)
authorizerDescription_status = Lens.lens (\AuthorizerDescription' {status} -> status) (\s@AuthorizerDescription' {} a -> s {status = a} :: AuthorizerDescription)

-- | The authorizer\'s Lambda function ARN.
authorizerDescription_authorizerFunctionArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerFunctionArn = Lens.lens (\AuthorizerDescription' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@AuthorizerDescription' {} a -> s {authorizerFunctionArn = a} :: AuthorizerDescription)

-- | Specifies whether IoT validates the token signature in an authorization
-- request.
authorizerDescription_signingDisabled :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Bool)
authorizerDescription_signingDisabled = Lens.lens (\AuthorizerDescription' {signingDisabled} -> signingDisabled) (\s@AuthorizerDescription' {} a -> s {signingDisabled = a} :: AuthorizerDescription)

-- | The public keys used to validate the token signature returned by your
-- custom authentication service.
authorizerDescription_tokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authorizerDescription_tokenSigningPublicKeys = Lens.lens (\AuthorizerDescription' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@AuthorizerDescription' {} a -> s {tokenSigningPublicKeys = a} :: AuthorizerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The authorizer ARN.
authorizerDescription_authorizerArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerArn = Lens.lens (\AuthorizerDescription' {authorizerArn} -> authorizerArn) (\s@AuthorizerDescription' {} a -> s {authorizerArn = a} :: AuthorizerDescription)

-- | When @true@, the result from the authorizer’s Lambda function is cached
-- for the time specified in @refreshAfterInSeconds@. The cached result is
-- used while the device reuses the same HTTP connection.
authorizerDescription_enableCachingForHttp :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Bool)
authorizerDescription_enableCachingForHttp = Lens.lens (\AuthorizerDescription' {enableCachingForHttp} -> enableCachingForHttp) (\s@AuthorizerDescription' {} a -> s {enableCachingForHttp = a} :: AuthorizerDescription)

-- | The authorizer name.
authorizerDescription_authorizerName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerName = Lens.lens (\AuthorizerDescription' {authorizerName} -> authorizerName) (\s@AuthorizerDescription' {} a -> s {authorizerName = a} :: AuthorizerDescription)

instance Core.FromJSON AuthorizerDescription where
  parseJSON =
    Core.withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            Prelude.<$> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "tokenKeyName")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "authorizerFunctionArn")
            Prelude.<*> (x Core..:? "signingDisabled")
            Prelude.<*> ( x Core..:? "tokenSigningPublicKeys"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "authorizerArn")
            Prelude.<*> (x Core..:? "enableCachingForHttp")
            Prelude.<*> (x Core..:? "authorizerName")
      )

instance Prelude.Hashable AuthorizerDescription where
  hashWithSalt _salt AuthorizerDescription' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` tokenKeyName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` authorizerFunctionArn
      `Prelude.hashWithSalt` signingDisabled
      `Prelude.hashWithSalt` tokenSigningPublicKeys
      `Prelude.hashWithSalt` authorizerArn
      `Prelude.hashWithSalt` enableCachingForHttp
      `Prelude.hashWithSalt` authorizerName

instance Prelude.NFData AuthorizerDescription where
  rnf AuthorizerDescription' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf tokenKeyName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf authorizerFunctionArn
      `Prelude.seq` Prelude.rnf signingDisabled
      `Prelude.seq` Prelude.rnf tokenSigningPublicKeys
      `Prelude.seq` Prelude.rnf authorizerArn
      `Prelude.seq` Prelude.rnf enableCachingForHttp
      `Prelude.seq` Prelude.rnf authorizerName
