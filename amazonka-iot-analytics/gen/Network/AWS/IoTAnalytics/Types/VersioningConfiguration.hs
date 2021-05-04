{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON VersioningConfiguration where
  parseJSON =
    Prelude.withObject
      "VersioningConfiguration"
      ( \x ->
          VersioningConfiguration'
            Prelude.<$> (x Prelude..:? "maxVersions")
            Prelude.<*> (x Prelude..:? "unlimited")
      )

instance Prelude.Hashable VersioningConfiguration

instance Prelude.NFData VersioningConfiguration

instance Prelude.ToJSON VersioningConfiguration where
  toJSON VersioningConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("maxVersions" Prelude..=) Prelude.<$> maxVersions,
            ("unlimited" Prelude..=) Prelude.<$> unlimited
          ]
      )
