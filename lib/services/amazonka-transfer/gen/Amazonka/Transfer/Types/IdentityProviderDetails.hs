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

-- | Returns information related to the type of user authentication that is
-- in use for a file transfer protocol-enabled server\'s users. A server
-- can have only one method of authentication.
--
-- /See:/ 'newIdentityProviderDetails' smart constructor.
data IdentityProviderDetails = IdentityProviderDetails'
  { -- | The identifier of the Directory Service directory that you want to stop
    -- sharing.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for a lambda function to use for the Identity provider.
    function :: Prelude.Maybe Prelude.Text,
    -- | Provides the type of @InvocationRole@ used to authenticate the user
    -- account.
    invocationRole :: Prelude.Maybe Prelude.Text,
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
-- 'function', 'identityProviderDetails_function' - The ARN for a lambda function to use for the Identity provider.
--
-- 'invocationRole', 'identityProviderDetails_invocationRole' - Provides the type of @InvocationRole@ used to authenticate the user
-- account.
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
      url = Prelude.Nothing
    }

-- | The identifier of the Directory Service directory that you want to stop
-- sharing.
identityProviderDetails_directoryId :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_directoryId = Lens.lens (\IdentityProviderDetails' {directoryId} -> directoryId) (\s@IdentityProviderDetails' {} a -> s {directoryId = a} :: IdentityProviderDetails)

-- | The ARN for a lambda function to use for the Identity provider.
identityProviderDetails_function :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_function = Lens.lens (\IdentityProviderDetails' {function} -> function) (\s@IdentityProviderDetails' {} a -> s {function = a} :: IdentityProviderDetails)

-- | Provides the type of @InvocationRole@ used to authenticate the user
-- account.
identityProviderDetails_invocationRole :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_invocationRole = Lens.lens (\IdentityProviderDetails' {invocationRole} -> invocationRole) (\s@IdentityProviderDetails' {} a -> s {invocationRole = a} :: IdentityProviderDetails)

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
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable IdentityProviderDetails where
  hashWithSalt _salt IdentityProviderDetails' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` invocationRole
      `Prelude.hashWithSalt` url

instance Prelude.NFData IdentityProviderDetails where
  rnf IdentityProviderDetails' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf function
      `Prelude.seq` Prelude.rnf invocationRole
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON IdentityProviderDetails where
  toJSON IdentityProviderDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("Function" Data..=) Prelude.<$> function,
            ("InvocationRole" Data..=)
              Prelude.<$> invocationRole,
            ("Url" Data..=) Prelude.<$> url
          ]
      )
