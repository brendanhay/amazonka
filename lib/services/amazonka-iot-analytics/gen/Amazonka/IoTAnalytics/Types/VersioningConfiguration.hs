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
-- Module      : Amazonka.IoTAnalytics.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.VersioningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the versioning of dataset contents.
--
-- /See:/ 'newVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { -- | How many versions of dataset contents are kept. The @unlimited@
    -- parameter must be @false@.
    maxVersions :: Prelude.Maybe Prelude.Natural,
    -- | If true, unlimited versions of dataset contents are kept.
    unlimited :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersioningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxVersions', 'versioningConfiguration_maxVersions' - How many versions of dataset contents are kept. The @unlimited@
-- parameter must be @false@.
--
-- 'unlimited', 'versioningConfiguration_unlimited' - If true, unlimited versions of dataset contents are kept.
newVersioningConfiguration ::
  VersioningConfiguration
newVersioningConfiguration =
  VersioningConfiguration'
    { maxVersions =
        Prelude.Nothing,
      unlimited = Prelude.Nothing
    }

-- | How many versions of dataset contents are kept. The @unlimited@
-- parameter must be @false@.
versioningConfiguration_maxVersions :: Lens.Lens' VersioningConfiguration (Prelude.Maybe Prelude.Natural)
versioningConfiguration_maxVersions = Lens.lens (\VersioningConfiguration' {maxVersions} -> maxVersions) (\s@VersioningConfiguration' {} a -> s {maxVersions = a} :: VersioningConfiguration)

-- | If true, unlimited versions of dataset contents are kept.
versioningConfiguration_unlimited :: Lens.Lens' VersioningConfiguration (Prelude.Maybe Prelude.Bool)
versioningConfiguration_unlimited = Lens.lens (\VersioningConfiguration' {unlimited} -> unlimited) (\s@VersioningConfiguration' {} a -> s {unlimited = a} :: VersioningConfiguration)

instance Data.FromJSON VersioningConfiguration where
  parseJSON =
    Data.withObject
      "VersioningConfiguration"
      ( \x ->
          VersioningConfiguration'
            Prelude.<$> (x Data..:? "maxVersions")
            Prelude.<*> (x Data..:? "unlimited")
      )

instance Prelude.Hashable VersioningConfiguration where
  hashWithSalt _salt VersioningConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` maxVersions
      `Prelude.hashWithSalt` unlimited

instance Prelude.NFData VersioningConfiguration where
  rnf VersioningConfiguration' {..} =
    Prelude.rnf maxVersions
      `Prelude.seq` Prelude.rnf unlimited

instance Data.ToJSON VersioningConfiguration where
  toJSON VersioningConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxVersions" Data..=) Prelude.<$> maxVersions,
            ("unlimited" Data..=) Prelude.<$> unlimited
          ]
      )
