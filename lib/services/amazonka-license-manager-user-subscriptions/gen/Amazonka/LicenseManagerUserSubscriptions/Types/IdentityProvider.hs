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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManagerUserSubscriptions.Types.ActiveDirectoryIdentityProvider
import qualified Amazonka.Prelude as Prelude

-- | Details about an identity provider.
--
-- /See:/ 'newIdentityProvider' smart constructor.
data IdentityProvider = IdentityProvider'
  { -- | An object that details an Active Directory identity provider.
    activeDirectoryIdentityProvider :: Prelude.Maybe ActiveDirectoryIdentityProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryIdentityProvider', 'identityProvider_activeDirectoryIdentityProvider' - An object that details an Active Directory identity provider.
newIdentityProvider ::
  IdentityProvider
newIdentityProvider =
  IdentityProvider'
    { activeDirectoryIdentityProvider =
        Prelude.Nothing
    }

-- | An object that details an Active Directory identity provider.
identityProvider_activeDirectoryIdentityProvider :: Lens.Lens' IdentityProvider (Prelude.Maybe ActiveDirectoryIdentityProvider)
identityProvider_activeDirectoryIdentityProvider = Lens.lens (\IdentityProvider' {activeDirectoryIdentityProvider} -> activeDirectoryIdentityProvider) (\s@IdentityProvider' {} a -> s {activeDirectoryIdentityProvider = a} :: IdentityProvider)

instance Core.FromJSON IdentityProvider where
  parseJSON =
    Core.withObject
      "IdentityProvider"
      ( \x ->
          IdentityProvider'
            Prelude.<$> (x Core..:? "ActiveDirectoryIdentityProvider")
      )

instance Prelude.Hashable IdentityProvider where
  hashWithSalt _salt IdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectoryIdentityProvider

instance Prelude.NFData IdentityProvider where
  rnf IdentityProvider' {..} =
    Prelude.rnf activeDirectoryIdentityProvider

instance Core.ToJSON IdentityProvider where
  toJSON IdentityProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ActiveDirectoryIdentityProvider" Core..=)
              Prelude.<$> activeDirectoryIdentityProvider
          ]
      )
