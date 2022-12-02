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
-- Module      : Amazonka.IoTWireless.Types.ProximityResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ProximityResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.SidewalkResourceTypeEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Proximity resource type event configuration object for enabling or
-- disabling topic.
--
-- /See:/ 'newProximityResourceTypeEventConfiguration' smart constructor.
data ProximityResourceTypeEventConfiguration = ProximityResourceTypeEventConfiguration'
  { -- | Proximity resource type event configuration object for enabling and
    -- disabling wireless device topic.
    sidewalk :: Prelude.Maybe SidewalkResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProximityResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'proximityResourceTypeEventConfiguration_sidewalk' - Proximity resource type event configuration object for enabling and
-- disabling wireless device topic.
newProximityResourceTypeEventConfiguration ::
  ProximityResourceTypeEventConfiguration
newProximityResourceTypeEventConfiguration =
  ProximityResourceTypeEventConfiguration'
    { sidewalk =
        Prelude.Nothing
    }

-- | Proximity resource type event configuration object for enabling and
-- disabling wireless device topic.
proximityResourceTypeEventConfiguration_sidewalk :: Lens.Lens' ProximityResourceTypeEventConfiguration (Prelude.Maybe SidewalkResourceTypeEventConfiguration)
proximityResourceTypeEventConfiguration_sidewalk = Lens.lens (\ProximityResourceTypeEventConfiguration' {sidewalk} -> sidewalk) (\s@ProximityResourceTypeEventConfiguration' {} a -> s {sidewalk = a} :: ProximityResourceTypeEventConfiguration)

instance
  Data.FromJSON
    ProximityResourceTypeEventConfiguration
  where
  parseJSON =
    Data.withObject
      "ProximityResourceTypeEventConfiguration"
      ( \x ->
          ProximityResourceTypeEventConfiguration'
            Prelude.<$> (x Data..:? "Sidewalk")
      )

instance
  Prelude.Hashable
    ProximityResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    ProximityResourceTypeEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    ProximityResourceTypeEventConfiguration
  where
  rnf ProximityResourceTypeEventConfiguration' {..} =
    Prelude.rnf sidewalk

instance
  Data.ToJSON
    ProximityResourceTypeEventConfiguration
  where
  toJSON ProximityResourceTypeEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Sidewalk" Data..=) Prelude.<$> sidewalk]
      )
