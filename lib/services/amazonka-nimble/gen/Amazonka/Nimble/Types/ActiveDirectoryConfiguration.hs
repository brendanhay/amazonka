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
-- Module      : Amazonka.Nimble.Types.ActiveDirectoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.ActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a Directory Service for Microsoft Active Directory
-- studio resource.
--
-- /See:/ 'newActiveDirectoryConfiguration' smart constructor.
data ActiveDirectoryConfiguration = ActiveDirectoryConfiguration'
  { -- | A collection of custom attributes for an Active Directory computer.
    computerAttributes :: Prelude.Maybe (Data.Sensitive [ActiveDirectoryComputerAttribute]),
    -- | The directory ID of the Directory Service for Microsoft Active Directory
    -- to access using this studio component.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The distinguished name (DN) and organizational unit (OU) of an Active
    -- Directory computer.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveDirectoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computerAttributes', 'activeDirectoryConfiguration_computerAttributes' - A collection of custom attributes for an Active Directory computer.
--
-- 'directoryId', 'activeDirectoryConfiguration_directoryId' - The directory ID of the Directory Service for Microsoft Active Directory
-- to access using this studio component.
--
-- 'organizationalUnitDistinguishedName', 'activeDirectoryConfiguration_organizationalUnitDistinguishedName' - The distinguished name (DN) and organizational unit (OU) of an Active
-- Directory computer.
newActiveDirectoryConfiguration ::
  ActiveDirectoryConfiguration
newActiveDirectoryConfiguration =
  ActiveDirectoryConfiguration'
    { computerAttributes =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      organizationalUnitDistinguishedName =
        Prelude.Nothing
    }

-- | A collection of custom attributes for an Active Directory computer.
activeDirectoryConfiguration_computerAttributes :: Lens.Lens' ActiveDirectoryConfiguration (Prelude.Maybe [ActiveDirectoryComputerAttribute])
activeDirectoryConfiguration_computerAttributes = Lens.lens (\ActiveDirectoryConfiguration' {computerAttributes} -> computerAttributes) (\s@ActiveDirectoryConfiguration' {} a -> s {computerAttributes = a} :: ActiveDirectoryConfiguration) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The directory ID of the Directory Service for Microsoft Active Directory
-- to access using this studio component.
activeDirectoryConfiguration_directoryId :: Lens.Lens' ActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
activeDirectoryConfiguration_directoryId = Lens.lens (\ActiveDirectoryConfiguration' {directoryId} -> directoryId) (\s@ActiveDirectoryConfiguration' {} a -> s {directoryId = a} :: ActiveDirectoryConfiguration)

-- | The distinguished name (DN) and organizational unit (OU) of an Active
-- Directory computer.
activeDirectoryConfiguration_organizationalUnitDistinguishedName :: Lens.Lens' ActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
activeDirectoryConfiguration_organizationalUnitDistinguishedName = Lens.lens (\ActiveDirectoryConfiguration' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@ActiveDirectoryConfiguration' {} a -> s {organizationalUnitDistinguishedName = a} :: ActiveDirectoryConfiguration)

instance Data.FromJSON ActiveDirectoryConfiguration where
  parseJSON =
    Data.withObject
      "ActiveDirectoryConfiguration"
      ( \x ->
          ActiveDirectoryConfiguration'
            Prelude.<$> ( x
                            Data..:? "computerAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "directoryId")
            Prelude.<*> (x Data..:? "organizationalUnitDistinguishedName")
      )

instance
  Prelude.Hashable
    ActiveDirectoryConfiguration
  where
  hashWithSalt _salt ActiveDirectoryConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` computerAttributes
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` organizationalUnitDistinguishedName

instance Prelude.NFData ActiveDirectoryConfiguration where
  rnf ActiveDirectoryConfiguration' {..} =
    Prelude.rnf computerAttributes
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedName

instance Data.ToJSON ActiveDirectoryConfiguration where
  toJSON ActiveDirectoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computerAttributes" Data..=)
              Prelude.<$> computerAttributes,
            ("directoryId" Data..=) Prelude.<$> directoryId,
            ("organizationalUnitDistinguishedName" Data..=)
              Prelude.<$> organizationalUnitDistinguishedName
          ]
      )
