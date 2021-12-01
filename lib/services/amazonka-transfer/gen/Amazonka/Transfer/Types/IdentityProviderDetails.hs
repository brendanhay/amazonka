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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.IdentityProviderDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information related to the type of user authentication that is
-- in use for a file transfer protocol-enabled server\'s users. A server
-- can have only one method of authentication.
--
-- /See:/ 'newIdentityProviderDetails' smart constructor.
data IdentityProviderDetails = IdentityProviderDetails'
  { -- | Provides the type of @InvocationRole@ used to authenticate the user
    -- account.
    invocationRole :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Web ServicesDirectory Service directory
    -- that you want to stop sharing.
    directoryId :: Prelude.Maybe Prelude.Text,
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
-- 'invocationRole', 'identityProviderDetails_invocationRole' - Provides the type of @InvocationRole@ used to authenticate the user
-- account.
--
-- 'directoryId', 'identityProviderDetails_directoryId' - The identifier of the Amazon Web ServicesDirectory Service directory
-- that you want to stop sharing.
--
-- 'url', 'identityProviderDetails_url' - Provides the location of the service endpoint used to authenticate
-- users.
newIdentityProviderDetails ::
  IdentityProviderDetails
newIdentityProviderDetails =
  IdentityProviderDetails'
    { invocationRole =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Provides the type of @InvocationRole@ used to authenticate the user
-- account.
identityProviderDetails_invocationRole :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_invocationRole = Lens.lens (\IdentityProviderDetails' {invocationRole} -> invocationRole) (\s@IdentityProviderDetails' {} a -> s {invocationRole = a} :: IdentityProviderDetails)

-- | The identifier of the Amazon Web ServicesDirectory Service directory
-- that you want to stop sharing.
identityProviderDetails_directoryId :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_directoryId = Lens.lens (\IdentityProviderDetails' {directoryId} -> directoryId) (\s@IdentityProviderDetails' {} a -> s {directoryId = a} :: IdentityProviderDetails)

-- | Provides the location of the service endpoint used to authenticate
-- users.
identityProviderDetails_url :: Lens.Lens' IdentityProviderDetails (Prelude.Maybe Prelude.Text)
identityProviderDetails_url = Lens.lens (\IdentityProviderDetails' {url} -> url) (\s@IdentityProviderDetails' {} a -> s {url = a} :: IdentityProviderDetails)

instance Core.FromJSON IdentityProviderDetails where
  parseJSON =
    Core.withObject
      "IdentityProviderDetails"
      ( \x ->
          IdentityProviderDetails'
            Prelude.<$> (x Core..:? "InvocationRole")
            Prelude.<*> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "Url")
      )

instance Prelude.Hashable IdentityProviderDetails where
  hashWithSalt salt' IdentityProviderDetails' {..} =
    salt' `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` invocationRole

instance Prelude.NFData IdentityProviderDetails where
  rnf IdentityProviderDetails' {..} =
    Prelude.rnf invocationRole
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf directoryId

instance Core.ToJSON IdentityProviderDetails where
  toJSON IdentityProviderDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InvocationRole" Core..=)
              Prelude.<$> invocationRole,
            ("DirectoryId" Core..=) Prelude.<$> directoryId,
            ("Url" Core..=) Prelude.<$> url
          ]
      )
