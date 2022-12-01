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
-- Module      : Amazonka.GroundStation.Types.TrackingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.TrackingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.Criticality
import qualified Amazonka.Prelude as Prelude

-- | Object that determines whether tracking should be used during a contact
-- executed with this @Config@ in the mission profile.
--
-- /See:/ 'newTrackingConfig' smart constructor.
data TrackingConfig = TrackingConfig'
  { -- | Current setting for autotrack.
    autotrack :: Criticality
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrackingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autotrack', 'trackingConfig_autotrack' - Current setting for autotrack.
newTrackingConfig ::
  -- | 'autotrack'
  Criticality ->
  TrackingConfig
newTrackingConfig pAutotrack_ =
  TrackingConfig' {autotrack = pAutotrack_}

-- | Current setting for autotrack.
trackingConfig_autotrack :: Lens.Lens' TrackingConfig Criticality
trackingConfig_autotrack = Lens.lens (\TrackingConfig' {autotrack} -> autotrack) (\s@TrackingConfig' {} a -> s {autotrack = a} :: TrackingConfig)

instance Core.FromJSON TrackingConfig where
  parseJSON =
    Core.withObject
      "TrackingConfig"
      ( \x ->
          TrackingConfig' Prelude.<$> (x Core..: "autotrack")
      )

instance Prelude.Hashable TrackingConfig where
  hashWithSalt _salt TrackingConfig' {..} =
    _salt `Prelude.hashWithSalt` autotrack

instance Prelude.NFData TrackingConfig where
  rnf TrackingConfig' {..} = Prelude.rnf autotrack

instance Core.ToJSON TrackingConfig where
  toJSON TrackingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("autotrack" Core..= autotrack)]
      )
