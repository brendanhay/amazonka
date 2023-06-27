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
-- Module      : Amazonka.Transfer.Types.IdentityProviderDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.IdentityProviderDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.SftpAuthenticationMethods

-- | Returns information related to the type of user authentication that is
-- in use for a file transfer protocol-enabled server\'s users. A server
-- can have only one method of authentication.
--
-- /See:/ 'newIdentityProviderDetails' smart constructor.
data IdentityProviderDetails = IdentityProviderDetails'
  { -- | The identifier of the Directory Service directory that you want to stop
    -- sharing.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for a Lambda function to use for the Identity provider.
    function :: Prelude.Maybe Prelude.Text,
    -- | This parameter is only applicable if your @IdentityProviderType@ is
    -- @API_GATEWAY@. Provides the type of @InvocationRole@ used to
    -- authenticate the user account.
    invocationRole :: Prelude.Maybe Prelude.Text,
    -- | For SFTP-enabled servers, and for custom identity providers /only/, you
    -- can specify whether to authenticate using a password, SSH key pair, or
    -- both.
    --
    -- -   @PASSWORD@ - users must provide their password to connect.
    --
    -- -   @PUBLIC_KEY@ - users must provide their private key to connect.
    --
    -- -   @PUBLIC_KEY_OR_PASSWORD@ - users can authenticate with either their
    --     password or their key. This is the default value.
    --
    -- -   @PUBLIC_KEY_AND_PASSWORD@ - users must provide both their private
    --     key and their password to connect. The server checks the key first,
    --     and then if the key is valid, the system prompts for a password. If
    --     the private key provided does not match the public key that is
    --     stored, authentication fails.
    sftpAuthenticationMethods :: Prelude.Maybe SftpAuthenticationMethods,
    -- | Provides the location of the service endpoint used to authenticate
    -- users.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProviderDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'identityProviderDetails_directoryId' - The identifier of the Directory Service directory that you want to stop
-- sharing.
--
-- 'function', 'identityProviderDetails_function' - The ARN for a Lambda function to use for the Identity provider.
--
-- 'invocationRole', 'identityProviderDetails_invocationRole' - This parameter is only applicable if your @IdentityProviderType@ is
-- @API_GATEWAY@. Provides the type of @InvocationRole@ used to
-- authenticate the user account.
--
-- 'sftpAuthenticationMethods', 'identityProviderDetails_sftpAuthenticationMethods' - For SFTP-enabled servers, and for custom identity providers /only/, you
-- can specify whether to authenticate using a password, SSH key pair, or
-- both.
--
-- -   @PASSWORD@ - users must provide their password to connect.
--
-- -   @PUBLIC_KEY@ - users must provide their private key to connect.
--
-- -   @PUBLIC_KEY_OR_PASSWORD@ - users can authenticate with either their
--     password or their key. This is the default value.
--
-- -   @PUBLIC_KEY_AND_PASSWORD@ - users must provide both their private
--     key and their password to connect. The server checks the key first,
--     and then if the key is valid, the system prompts for a password. If
--     the private key provided does not match the public key that is
--     stored, authentication fails.
--
-- 'url', 'identityProviderDetails_url' - Provides the location of the service endpoint used to authenticate
-- users.
newIdentityProviderDetails ::
  IdentityProviderDetails
newIdentityProviderDetails =
  IdentityProviderDetails'
    { directoryId =
        Prelude.Nothing,
      function = Prelude.Nothing,
      invocationRole = Prelude.Nothing,
      sftpAuthenticationMethods = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The identifier of the Directory Service directory that you want to stop
-- sharing.
identityProviderDetails_directoryId :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_directoryId = Lens.lens (\IdentityProviderDetails' {directoryId} -> directoryId) (\s@IdentityProviderDetails' {} a -> s {directoryId = a} :: IdentityProviderDetails)

-- | The ARN for a Lambda function to use for the Identity provider.
identityProviderDetails_function :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_function = Lens.lens (\IdentityProviderDetails' {function} -> function) (\s@IdentityProviderDetails' {} a -> s {function = a} :: IdentityProviderDetails)

-- | This parameter is only applicable if your @IdentityProviderType@ is
-- @API_GATEWAY@. Provides the type of @InvocationRole@ used to
-- authenticate the user account.
identityProviderDetails_invocationRole :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_invocationRole = Lens.lens (\IdentityProviderDetails' {invocationRole} -> invocationRole) (\s@IdentityProviderDetails' {} a -> s {invocationRole = a} :: IdentityProviderDetails)

-- | For SFTP-enabled servers, and for custom identity providers /only/, you
-- can specify whether to authenticate using a password, SSH key pair, or
-- both.
--
-- -   @PASSWORD@ - users must provide their password to connect.
--
-- -   @PUBLIC_KEY@ - users must provide their private key to connect.
--
-- -   @PUBLIC_KEY_OR_PASSWORD@ - users can authenticate with either their
--     password or their key. This is the default value.
--
-- -   @PUBLIC_KEY_AND_PASSWORD@ - users must provide both their private
--     key and their password to connect. The server checks the key first,
--     and then if the key is valid, the system prompts for a password. If
--     the private key provided does not match the public key that is
--     stored, authentication fails.
identityProviderDetails_sftpAuthenticationMethods :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe SftpAuthenticationMethods)
identityProviderDetails_sftpAuthenticationMethods = Lens.lens (\IdentityProviderDetails' {sftpAuthenticationMethods} -> sftpAuthenticationMethods) (\s@IdentityProviderDetails' {} a -> s {sftpAuthenticationMethods = a} :: IdentityProviderDetails)

-- | Provides the location of the service endpoint used to authenticate
-- users.
identityProviderDetails_url :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_url = Lens.lens (\IdentityProviderDetails' {url} -> url) (\s@IdentityProviderDetails' {} a -> s {url = a} :: IdentityProviderDetails)

instance Data.FromJSON IdentityProviderDetails where
  parseJSON =
    Data.withObject
      "IdentityProviderDetails"
      ( \x ->
          IdentityProviderDetails'
            Prelude.<$> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "Function")
            Prelude.<*> (x Data..:? "InvocationRole")
            Prelude.<*> (x Data..:? "SftpAuthenticationMethods")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable IdentityProviderDetails where
  hashWithSalt _salt IdentityProviderDetails' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` invocationRole
      `Prelude.hashWithSalt` sftpAuthenticationMethods
      `Prelude.hashWithSalt` url

instance Prelude.NFData IdentityProviderDetails where
  rnf IdentityProviderDetails' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf function
      `Prelude.seq` Prelude.rnf invocationRole
      `Prelude.seq` Prelude.rnf sftpAuthenticationMethods
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON IdentityProviderDetails where
  toJSON IdentityProviderDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("Function" Data..=) Prelude.<$> function,
            ("InvocationRole" Data..=)
              Prelude.<$> invocationRole,
            ("SftpAuthenticationMethods" Data..=)
              Prelude.<$> sftpAuthenticationMethods,
            ("Url" Data..=) Prelude.<$> url
          ]
      )
