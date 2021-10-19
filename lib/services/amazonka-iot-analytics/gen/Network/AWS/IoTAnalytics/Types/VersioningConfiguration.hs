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
-- Module      : Network.AWS.IoTAnalytics.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.VersioningConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the versioning of dataset contents.
--
-- /See:/ 'newVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { -- | If true, unlimited versions of dataset contents are kept.
    unlimited :: Prelude.Maybe Prelude.Bool,
    -- | How many versions of dataset contents are kept. The @unlimited@
    -- parameter must be @false@.
    maxVersions :: Prelude.Maybe Prelude.Natural
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
-- 'unlimited', 'versioningConfiguration_unlimited' - If true, unlimited versions of dataset contents are kept.
--
-- 'maxVersions', 'versioningConfiguration_maxVersions' - How many versions of dataset contents are kept. The @unlimited@
-- parameter must be @false@.
newVersioningConfiguration ::
  VersioningConfiguration
newVersioningConfiguration =
  VersioningConfiguration'
    { unlimited =
        Prelude.Nothing,
      maxVersions = Prelude.Nothing
    }

-- | If true, unlimited versions of dataset contents are kept.
versioningConfiguration_unlimited :: Lens.Lens' VersioningConfiguration (Prelude.Maybe Prelude.Bool)
versioningConfiguration_unlimited = Lens.lens (\VersioningConfiguration' {unlimited} -> unlimited) (\s@VersioningConfiguration' {} a -> s {unlimited = a} :: VersioningConfiguration)

-- | How many versions of dataset contents are kept. The @unlimited@
-- parameter must be @false@.
versioningConfiguration_maxVersions :: Lens.Lens' VersioningConfiguration (Prelude.Maybe Prelude.Natural)
versioningConfiguration_maxVersions = Lens.lens (\VersioningConfiguration' {maxVersions} -> maxVersions) (\s@VersioningConfiguration' {} a -> s {maxVersions = a} :: VersioningConfiguration)

instance Core.FromJSON VersioningConfiguration where
  parseJSON =
    Core.withObject
      "VersioningConfiguration"
      ( \x ->
          VersioningConfiguration'
            Prelude.<$> (x Core..:? "unlimited")
            Prelude.<*> (x Core..:? "maxVersions")
      )

instance Prelude.Hashable VersioningConfiguration

instance Prelude.NFData VersioningConfiguration

instance Core.ToJSON VersioningConfiguration where
  toJSON VersioningConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unlimited" Core..=) Prelude.<$> unlimited,
            ("maxVersions" Core..=) Prelude.<$> maxVersions
          ]
      )
