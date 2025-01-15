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
-- Module      : Amazonka.Nimble.Types.StudioComponentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.ActiveDirectoryConfiguration
import Amazonka.Nimble.Types.ComputeFarmConfiguration
import Amazonka.Nimble.Types.LicenseServiceConfiguration
import Amazonka.Nimble.Types.SharedFileSystemConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the studio component, based on component type.
--
-- /See:/ 'newStudioComponentConfiguration' smart constructor.
data StudioComponentConfiguration = StudioComponentConfiguration'
  { -- | The configuration for a Directory Service for Microsoft Active Directory
    -- studio resource.
    activeDirectoryConfiguration :: Prelude.Maybe ActiveDirectoryConfiguration,
    -- | The configuration for a render farm that is associated with a studio
    -- resource.
    computeFarmConfiguration :: Prelude.Maybe ComputeFarmConfiguration,
    -- | The configuration for a license service that is associated with a studio
    -- resource.
    licenseServiceConfiguration :: Prelude.Maybe LicenseServiceConfiguration,
    -- | The configuration for a shared file storage system that is associated
    -- with a studio resource.
    sharedFileSystemConfiguration :: Prelude.Maybe SharedFileSystemConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioComponentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryConfiguration', 'studioComponentConfiguration_activeDirectoryConfiguration' - The configuration for a Directory Service for Microsoft Active Directory
-- studio resource.
--
-- 'computeFarmConfiguration', 'studioComponentConfiguration_computeFarmConfiguration' - The configuration for a render farm that is associated with a studio
-- resource.
--
-- 'licenseServiceConfiguration', 'studioComponentConfiguration_licenseServiceConfiguration' - The configuration for a license service that is associated with a studio
-- resource.
--
-- 'sharedFileSystemConfiguration', 'studioComponentConfiguration_sharedFileSystemConfiguration' - The configuration for a shared file storage system that is associated
-- with a studio resource.
newStudioComponentConfiguration ::
  StudioComponentConfiguration
newStudioComponentConfiguration =
  StudioComponentConfiguration'
    { activeDirectoryConfiguration =
        Prelude.Nothing,
      computeFarmConfiguration = Prelude.Nothing,
      licenseServiceConfiguration = Prelude.Nothing,
      sharedFileSystemConfiguration =
        Prelude.Nothing
    }

-- | The configuration for a Directory Service for Microsoft Active Directory
-- studio resource.
studioComponentConfiguration_activeDirectoryConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe ActiveDirectoryConfiguration)
studioComponentConfiguration_activeDirectoryConfiguration = Lens.lens (\StudioComponentConfiguration' {activeDirectoryConfiguration} -> activeDirectoryConfiguration) (\s@StudioComponentConfiguration' {} a -> s {activeDirectoryConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a render farm that is associated with a studio
-- resource.
studioComponentConfiguration_computeFarmConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe ComputeFarmConfiguration)
studioComponentConfiguration_computeFarmConfiguration = Lens.lens (\StudioComponentConfiguration' {computeFarmConfiguration} -> computeFarmConfiguration) (\s@StudioComponentConfiguration' {} a -> s {computeFarmConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a license service that is associated with a studio
-- resource.
studioComponentConfiguration_licenseServiceConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe LicenseServiceConfiguration)
studioComponentConfiguration_licenseServiceConfiguration = Lens.lens (\StudioComponentConfiguration' {licenseServiceConfiguration} -> licenseServiceConfiguration) (\s@StudioComponentConfiguration' {} a -> s {licenseServiceConfiguration = a} :: StudioComponentConfiguration)

-- | The configuration for a shared file storage system that is associated
-- with a studio resource.
studioComponentConfiguration_sharedFileSystemConfiguration :: Lens.Lens' StudioComponentConfiguration (Prelude.Maybe SharedFileSystemConfiguration)
studioComponentConfiguration_sharedFileSystemConfiguration = Lens.lens (\StudioComponentConfiguration' {sharedFileSystemConfiguration} -> sharedFileSystemConfiguration) (\s@StudioComponentConfiguration' {} a -> s {sharedFileSystemConfiguration = a} :: StudioComponentConfiguration)

instance Data.FromJSON StudioComponentConfiguration where
  parseJSON =
    Data.withObject
      "StudioComponentConfiguration"
      ( \x ->
          StudioComponentConfiguration'
            Prelude.<$> (x Data..:? "activeDirectoryConfiguration")
            Prelude.<*> (x Data..:? "computeFarmConfiguration")
            Prelude.<*> (x Data..:? "licenseServiceConfiguration")
            Prelude.<*> (x Data..:? "sharedFileSystemConfiguration")
      )

instance
  Prelude.Hashable
    StudioComponentConfiguration
  where
  hashWithSalt _salt StudioComponentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectoryConfiguration
      `Prelude.hashWithSalt` computeFarmConfiguration
      `Prelude.hashWithSalt` licenseServiceConfiguration
      `Prelude.hashWithSalt` sharedFileSystemConfiguration

instance Prelude.NFData StudioComponentConfiguration where
  rnf StudioComponentConfiguration' {..} =
    Prelude.rnf activeDirectoryConfiguration `Prelude.seq`
      Prelude.rnf computeFarmConfiguration `Prelude.seq`
        Prelude.rnf licenseServiceConfiguration `Prelude.seq`
          Prelude.rnf sharedFileSystemConfiguration

instance Data.ToJSON StudioComponentConfiguration where
  toJSON StudioComponentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("activeDirectoryConfiguration" Data..=)
              Prelude.<$> activeDirectoryConfiguration,
            ("computeFarmConfiguration" Data..=)
              Prelude.<$> computeFarmConfiguration,
            ("licenseServiceConfiguration" Data..=)
              Prelude.<$> licenseServiceConfiguration,
            ("sharedFileSystemConfiguration" Data..=)
              Prelude.<$> sharedFileSystemConfiguration
          ]
      )
