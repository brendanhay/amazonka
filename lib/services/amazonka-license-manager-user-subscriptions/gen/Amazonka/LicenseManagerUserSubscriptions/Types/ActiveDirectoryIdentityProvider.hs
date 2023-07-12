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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.ActiveDirectoryIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.ActiveDirectoryIdentityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an Active Directory identity provider.
--
-- /See:/ 'newActiveDirectoryIdentityProvider' smart constructor.
data ActiveDirectoryIdentityProvider = ActiveDirectoryIdentityProvider'
  { -- | The directory ID for an Active Directory identity provider.
    directoryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveDirectoryIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'activeDirectoryIdentityProvider_directoryId' - The directory ID for an Active Directory identity provider.
newActiveDirectoryIdentityProvider ::
  ActiveDirectoryIdentityProvider
newActiveDirectoryIdentityProvider =
  ActiveDirectoryIdentityProvider'
    { directoryId =
        Prelude.Nothing
    }

-- | The directory ID for an Active Directory identity provider.
activeDirectoryIdentityProvider_directoryId :: Lens.Lens' ActiveDirectoryIdentityProvider (Prelude.Maybe Prelude.Text)
activeDirectoryIdentityProvider_directoryId = Lens.lens (\ActiveDirectoryIdentityProvider' {directoryId} -> directoryId) (\s@ActiveDirectoryIdentityProvider' {} a -> s {directoryId = a} :: ActiveDirectoryIdentityProvider)

instance
  Data.FromJSON
    ActiveDirectoryIdentityProvider
  where
  parseJSON =
    Data.withObject
      "ActiveDirectoryIdentityProvider"
      ( \x ->
          ActiveDirectoryIdentityProvider'
            Prelude.<$> (x Data..:? "DirectoryId")
      )

instance
  Prelude.Hashable
    ActiveDirectoryIdentityProvider
  where
  hashWithSalt
    _salt
    ActiveDirectoryIdentityProvider' {..} =
      _salt `Prelude.hashWithSalt` directoryId

instance
  Prelude.NFData
    ActiveDirectoryIdentityProvider
  where
  rnf ActiveDirectoryIdentityProvider' {..} =
    Prelude.rnf directoryId

instance Data.ToJSON ActiveDirectoryIdentityProvider where
  toJSON ActiveDirectoryIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [("DirectoryId" Data..=) Prelude.<$> directoryId]
      )
