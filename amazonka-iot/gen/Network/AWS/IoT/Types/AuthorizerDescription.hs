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
import qualified Network.AWS.Prelude as Prelude

-- | The authorizer description.
--
-- /See:/ 'newAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { -- | The UNIX timestamp of when the authorizer was last updated.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the authorizer.
    status :: Prelude.Maybe AuthorizerStatus,
    -- | The authorizer\'s Lambda function ARN.
    authorizerFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the authorizer was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The public keys used to validate the token signature returned by your
    -- custom authentication service.
    tokenSigningPublicKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text,
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether IoT validates the token signature in an authorization
    -- request.
    signingDisabled :: Prelude.Maybe Prelude.Bool
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
-- 'authorizerArn', 'authorizerDescription_authorizerArn' - The authorizer ARN.
--
-- 'status', 'authorizerDescription_status' - The status of the authorizer.
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
-- 'tokenKeyName', 'authorizerDescription_tokenKeyName' - The key used to extract the token from the HTTP headers.
--
-- 'signingDisabled', 'authorizerDescription_signingDisabled' - Specifies whether IoT validates the token signature in an authorization
-- request.
newAuthorizerDescription ::
  AuthorizerDescription
newAuthorizerDescription =
  AuthorizerDescription'
    { lastModifiedDate =
        Prelude.Nothing,
      authorizerArn = Prelude.Nothing,
      status = Prelude.Nothing,
      authorizerFunctionArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      tokenSigningPublicKeys = Prelude.Nothing,
      authorizerName = Prelude.Nothing,
      tokenKeyName = Prelude.Nothing,
      signingDisabled = Prelude.Nothing
    }

-- | The UNIX timestamp of when the authorizer was last updated.
authorizerDescription_lastModifiedDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_lastModifiedDate = Lens.lens (\AuthorizerDescription' {lastModifiedDate} -> lastModifiedDate) (\s@AuthorizerDescription' {} a -> s {lastModifiedDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Core._Time

-- | The authorizer ARN.
authorizerDescription_authorizerArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerArn = Lens.lens (\AuthorizerDescription' {authorizerArn} -> authorizerArn) (\s@AuthorizerDescription' {} a -> s {authorizerArn = a} :: AuthorizerDescription)

-- | The status of the authorizer.
authorizerDescription_status :: Lens.Lens' AuthorizerDescription (Prelude.Maybe AuthorizerStatus)
authorizerDescription_status = Lens.lens (\AuthorizerDescription' {status} -> status) (\s@AuthorizerDescription' {} a -> s {status = a} :: AuthorizerDescription)

-- | The authorizer\'s Lambda function ARN.
authorizerDescription_authorizerFunctionArn :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerFunctionArn = Lens.lens (\AuthorizerDescription' {authorizerFunctionArn} -> authorizerFunctionArn) (\s@AuthorizerDescription' {} a -> s {authorizerFunctionArn = a} :: AuthorizerDescription)

-- | The UNIX timestamp of when the authorizer was created.
authorizerDescription_creationDate :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.UTCTime)
authorizerDescription_creationDate = Lens.lens (\AuthorizerDescription' {creationDate} -> creationDate) (\s@AuthorizerDescription' {} a -> s {creationDate = a} :: AuthorizerDescription) Prelude.. Lens.mapping Core._Time

-- | The public keys used to validate the token signature returned by your
-- custom authentication service.
authorizerDescription_tokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
authorizerDescription_tokenSigningPublicKeys = Lens.lens (\AuthorizerDescription' {tokenSigningPublicKeys} -> tokenSigningPublicKeys) (\s@AuthorizerDescription' {} a -> s {tokenSigningPublicKeys = a} :: AuthorizerDescription) Prelude.. Lens.mapping Lens._Coerce

-- | The authorizer name.
authorizerDescription_authorizerName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_authorizerName = Lens.lens (\AuthorizerDescription' {authorizerName} -> authorizerName) (\s@AuthorizerDescription' {} a -> s {authorizerName = a} :: AuthorizerDescription)

-- | The key used to extract the token from the HTTP headers.
authorizerDescription_tokenKeyName :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Text)
authorizerDescription_tokenKeyName = Lens.lens (\AuthorizerDescription' {tokenKeyName} -> tokenKeyName) (\s@AuthorizerDescription' {} a -> s {tokenKeyName = a} :: AuthorizerDescription)

-- | Specifies whether IoT validates the token signature in an authorization
-- request.
authorizerDescription_signingDisabled :: Lens.Lens' AuthorizerDescription (Prelude.Maybe Prelude.Bool)
authorizerDescription_signingDisabled = Lens.lens (\AuthorizerDescription' {signingDisabled} -> signingDisabled) (\s@AuthorizerDescription' {} a -> s {signingDisabled = a} :: AuthorizerDescription)

instance Core.FromJSON AuthorizerDescription where
  parseJSON =
    Core.withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            Prelude.<$> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "authorizerArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "authorizerFunctionArn")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> ( x Core..:? "tokenSigningPublicKeys"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "authorizerName")
            Prelude.<*> (x Core..:? "tokenKeyName")
            Prelude.<*> (x Core..:? "signingDisabled")
      )

instance Prelude.Hashable AuthorizerDescription

instance Prelude.NFData AuthorizerDescription
