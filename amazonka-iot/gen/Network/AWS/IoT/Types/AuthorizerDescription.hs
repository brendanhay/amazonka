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
-- Module      : Network.AWS.IoT.Types.AuthorizerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AuthorizerStatus
import qualified Network.AWS.Lens as Lens

-- | The authorizer description.
--
-- /See:/ 'newAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { -- | The UNIX timestamp of when the authorizer was last updated.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The status of the authorizer.
    status :: Core.Maybe AuthorizerStatus,
    -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Core.Text,
    -- | The authorizer\'s Lambda function ARN.
    authorizerFunctionArn :: Core.Maybe Core.Text,
    -- | The UNIX timestamp of when the authorizer was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The public keys used to validate the token signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The authorizer name.
    authorizerName :: Core.Maybe Core.Text,
    -- | Specifies whether AWS IoT validates the token signature in an
    -- authorization request.
    signingDisabled :: Core.Maybe Core.Bool,
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'status', 'authorizerDescription_status' - The status of the authorizer.
--
-- 'authorizerArn', 'authorizerDescription_authorizerArn' - The authorizer ARN.
--
-- 'authorizerFunctionArn', 'authorizerDescription_authorizerFunctionArn' - The authorizer\'s Lambda function ARN.
--
-- 'creationDate', 'authorizerDescription_creationDate' - The UNIX timestamp of when the authorizer was created.
--
-- 'tokenSigningPublicKeys', 'authorizerDescription_tokenSigningPublicKeys' - The public keys used to validate the token signature returned by your
-- custom authentication service.
--
-- 'authorizerName', 'authorizerDescription_authorizerName' - The authorizer name.
--
-- 'signingDisabled', 'authorizerDescription_signingDisabled' - Specifies whether AWS IoT validates the token signature in an
-- authorization request.
--
-- 'tokenKeyName', 'authorizerDescription_tokenKeyName' - The key used to extract the token from the HTTP headers.
newAuthorizerDescription ::
  AuthorizerDescription
newAuthorizerDescription =
  AuthorizerDescription'
    { lastModifiedDate =
        Core.Nothing,
      status = Core.Nothing,
      authorizerArn = Core.Nothing,
      authorizerFunctionArn = Core.Nothing,
      creationDate = Core.Nothing,
      tokenSigningPublicKeys = Core.Nothing,
      authorizerName = Core.Nothing,
      signingDisabled = Core.Nothing,
      tokenKeyName = Core.Nothing
    }

-- | The UNIX timestamp of when the authorizer was last updated.
authorizerDescription_lastModifiedDate :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.UTCTime)
authorizerDescription_lastModifiedDate = Lens.lens (\AuthorizerDescription' {lastModifiedDate} -> lastModifiedDate) (\s@AuthorizerDescription' {} a -> s {lastModifiedDate = a} :: AuthorizerDescription) Core.. Lens.mapping Core._Time

-- | The status of the authorizer.
authorizerDescription_status :: Lens.Lens' AuthorizerDescription (Core.Maybe AuthorizerStatus)
authorizerDescription_status = Lens.lens (\AuthorizerDescription' {status} -> status) (\s@AuthorizerDescription' {} a -> s {status = a} :: AuthorizerDescription)

-- | The authorizer ARN.
authorizerDescription_authorizerArn :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Text)
authorizerDescription_authorizerArn = Lens.lens (\AuthorizerDescription' {authorizerArn} -> authorizerArn) (\s@AuthorizerDescription' {} a -> s {authorizerArn = a} :: AuthorizerDescription)

-- | The authorizer\'s Lambda function ARN.
authorizerDescription_authorizerFunctionArn :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Text)
authorizerDescription_authorizerFunctionArn = Lens.lens (\AuthorizerDescription' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@AuthorizerDescription' {} a -> s {authorizerFunctionArn = a} :: AuthorizerDescription)

-- | The UNIX timestamp of when the authorizer was created.
authorizerDescription_creationDate :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.UTCTime)
authorizerDescription_creationDate = Lens.lens (\AuthorizerDescription' {creationDate} -> creationDate) (\s@AuthorizerDescription' {} a -> s {creationDate = a} :: AuthorizerDescription) Core.. Lens.mapping Core._Time

-- | The public keys used to validate the token signature returned by your
-- custom authentication service.
authorizerDescription_tokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Core.Maybe (Core.HashMap Core.Text Core.Text))
authorizerDescription_tokenSigningPublicKeys = Lens.lens (\AuthorizerDescription' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@AuthorizerDescription' {} a -> s {tokenSigningPublicKeys = a} :: AuthorizerDescription) Core.. Lens.mapping Lens._Coerce

-- | The authorizer name.
authorizerDescription_authorizerName :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Text)
authorizerDescription_authorizerName = Lens.lens (\AuthorizerDescription' {authorizerName} -> authorizerName) (\s@AuthorizerDescription' {} a -> s {authorizerName = a} :: AuthorizerDescription)

-- | Specifies whether AWS IoT validates the token signature in an
-- authorization request.
authorizerDescription_signingDisabled :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Bool)
authorizerDescription_signingDisabled = Lens.lens (\AuthorizerDescription' {signingDisabled} -> signingDisabled) (\s@AuthorizerDescription' {} a -> s {signingDisabled = a} :: AuthorizerDescription)

-- | The key used to extract the token from the HTTP headers.
authorizerDescription_tokenKeyName :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Text)
authorizerDescription_tokenKeyName = Lens.lens (\AuthorizerDescription' {tokenKeyName} -> tokenKeyName) (\s@AuthorizerDescription' {} a -> s {tokenKeyName = a} :: AuthorizerDescription)

instance Core.FromJSON AuthorizerDescription where
  parseJSON =
    Core.withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            Core.<$> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "authorizerArn")
            Core.<*> (x Core..:? "authorizerFunctionArn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> ( x Core..:? "tokenSigningPublicKeys"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "authorizerName")
            Core.<*> (x Core..:? "signingDisabled")
            Core.<*> (x Core..:? "tokenKeyName")
      )

instance Core.Hashable AuthorizerDescription

instance Core.NFData AuthorizerDescription
