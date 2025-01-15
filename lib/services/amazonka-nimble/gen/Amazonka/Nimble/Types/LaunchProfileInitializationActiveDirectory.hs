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
-- Module      : Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileInitializationActiveDirectory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.ActiveDirectoryComputerAttribute
import qualified Amazonka.Prelude as Prelude

-- | The launch profile initialization Active Directory contains information
-- required for the launch profile to connect to the Active Directory.
--
-- /See:/ 'newLaunchProfileInitializationActiveDirectory' smart constructor.
data LaunchProfileInitializationActiveDirectory = LaunchProfileInitializationActiveDirectory'
  { -- | A collection of custom attributes for an Active Directory computer.
    computerAttributes :: Prelude.Maybe (Data.Sensitive [ActiveDirectoryComputerAttribute]),
    -- | The directory ID of the Directory Service for Microsoft Active Directory
    -- to access using this launch profile.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The directory name.
    directoryName :: Prelude.Maybe Prelude.Text,
    -- | The DNS IP address.
    dnsIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The name for the organizational unit distinguished name.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a studio component resource.
    studioComponentId :: Prelude.Maybe Prelude.Text,
    -- | The name for the studio component.
    studioComponentName :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileInitializationActiveDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computerAttributes', 'launchProfileInitializationActiveDirectory_computerAttributes' - A collection of custom attributes for an Active Directory computer.
--
-- 'directoryId', 'launchProfileInitializationActiveDirectory_directoryId' - The directory ID of the Directory Service for Microsoft Active Directory
-- to access using this launch profile.
--
-- 'directoryName', 'launchProfileInitializationActiveDirectory_directoryName' - The directory name.
--
-- 'dnsIpAddresses', 'launchProfileInitializationActiveDirectory_dnsIpAddresses' - The DNS IP address.
--
-- 'organizationalUnitDistinguishedName', 'launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName' - The name for the organizational unit distinguished name.
--
-- 'studioComponentId', 'launchProfileInitializationActiveDirectory_studioComponentId' - The unique identifier for a studio component resource.
--
-- 'studioComponentName', 'launchProfileInitializationActiveDirectory_studioComponentName' - The name for the studio component.
newLaunchProfileInitializationActiveDirectory ::
  LaunchProfileInitializationActiveDirectory
newLaunchProfileInitializationActiveDirectory =
  LaunchProfileInitializationActiveDirectory'
    { computerAttributes =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      directoryName = Prelude.Nothing,
      dnsIpAddresses =
        Prelude.Nothing,
      organizationalUnitDistinguishedName =
        Prelude.Nothing,
      studioComponentId =
        Prelude.Nothing,
      studioComponentName =
        Prelude.Nothing
    }

-- | A collection of custom attributes for an Active Directory computer.
launchProfileInitializationActiveDirectory_computerAttributes :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe [ActiveDirectoryComputerAttribute])
launchProfileInitializationActiveDirectory_computerAttributes = Lens.lens (\LaunchProfileInitializationActiveDirectory' {computerAttributes} -> computerAttributes) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {computerAttributes = a} :: LaunchProfileInitializationActiveDirectory) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The directory ID of the Directory Service for Microsoft Active Directory
-- to access using this launch profile.
launchProfileInitializationActiveDirectory_directoryId :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe Prelude.Text)
launchProfileInitializationActiveDirectory_directoryId = Lens.lens (\LaunchProfileInitializationActiveDirectory' {directoryId} -> directoryId) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {directoryId = a} :: LaunchProfileInitializationActiveDirectory)

-- | The directory name.
launchProfileInitializationActiveDirectory_directoryName :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe Prelude.Text)
launchProfileInitializationActiveDirectory_directoryName = Lens.lens (\LaunchProfileInitializationActiveDirectory' {directoryName} -> directoryName) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {directoryName = a} :: LaunchProfileInitializationActiveDirectory)

-- | The DNS IP address.
launchProfileInitializationActiveDirectory_dnsIpAddresses :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe [Prelude.Text])
launchProfileInitializationActiveDirectory_dnsIpAddresses = Lens.lens (\LaunchProfileInitializationActiveDirectory' {dnsIpAddresses} -> dnsIpAddresses) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {dnsIpAddresses = a} :: LaunchProfileInitializationActiveDirectory) Prelude.. Lens.mapping Lens.coerced

-- | The name for the organizational unit distinguished name.
launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe Prelude.Text)
launchProfileInitializationActiveDirectory_organizationalUnitDistinguishedName = Lens.lens (\LaunchProfileInitializationActiveDirectory' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {organizationalUnitDistinguishedName = a} :: LaunchProfileInitializationActiveDirectory)

-- | The unique identifier for a studio component resource.
launchProfileInitializationActiveDirectory_studioComponentId :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe Prelude.Text)
launchProfileInitializationActiveDirectory_studioComponentId = Lens.lens (\LaunchProfileInitializationActiveDirectory' {studioComponentId} -> studioComponentId) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {studioComponentId = a} :: LaunchProfileInitializationActiveDirectory)

-- | The name for the studio component.
launchProfileInitializationActiveDirectory_studioComponentName :: Lens.Lens' LaunchProfileInitializationActiveDirectory (Prelude.Maybe Prelude.Text)
launchProfileInitializationActiveDirectory_studioComponentName = Lens.lens (\LaunchProfileInitializationActiveDirectory' {studioComponentName} -> studioComponentName) (\s@LaunchProfileInitializationActiveDirectory' {} a -> s {studioComponentName = a} :: LaunchProfileInitializationActiveDirectory) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    LaunchProfileInitializationActiveDirectory
  where
  parseJSON =
    Data.withObject
      "LaunchProfileInitializationActiveDirectory"
      ( \x ->
          LaunchProfileInitializationActiveDirectory'
            Prelude.<$> ( x
                            Data..:? "computerAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "directoryId")
            Prelude.<*> (x Data..:? "directoryName")
            Prelude.<*> (x Data..:? "dnsIpAddresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "organizationalUnitDistinguishedName")
            Prelude.<*> (x Data..:? "studioComponentId")
            Prelude.<*> (x Data..:? "studioComponentName")
      )

instance
  Prelude.Hashable
    LaunchProfileInitializationActiveDirectory
  where
  hashWithSalt
    _salt
    LaunchProfileInitializationActiveDirectory' {..} =
      _salt
        `Prelude.hashWithSalt` computerAttributes
        `Prelude.hashWithSalt` directoryId
        `Prelude.hashWithSalt` directoryName
        `Prelude.hashWithSalt` dnsIpAddresses
        `Prelude.hashWithSalt` organizationalUnitDistinguishedName
        `Prelude.hashWithSalt` studioComponentId
        `Prelude.hashWithSalt` studioComponentName

instance
  Prelude.NFData
    LaunchProfileInitializationActiveDirectory
  where
  rnf LaunchProfileInitializationActiveDirectory' {..} =
    Prelude.rnf computerAttributes `Prelude.seq`
      Prelude.rnf directoryId `Prelude.seq`
        Prelude.rnf directoryName `Prelude.seq`
          Prelude.rnf dnsIpAddresses `Prelude.seq`
            Prelude.rnf organizationalUnitDistinguishedName `Prelude.seq`
              Prelude.rnf studioComponentId `Prelude.seq`
                Prelude.rnf studioComponentName
