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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneOptions where

import Network.AWS.ElasticSearch.Types.AutoTuneDesiredState
import Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Network.AWS.ElasticSearch.Types.RollbackOnDisable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the Auto-Tune options: the Auto-Tune desired state for the
-- domain, rollback state when disabling Auto-Tune options and list of
-- maintenance schedules.
--
-- /See:/ 'newAutoTuneOptions' smart constructor.
data AutoTuneOptions = AutoTuneOptions'
  { -- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
    -- DISABLED.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | Specifies the rollback state while disabling Auto-Tune for the domain.
    -- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
    rollbackOnDisable :: Prelude.Maybe RollbackOnDisable,
    -- | Specifies list of maitenance schedules. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'autoTuneOptions_desiredState' - Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
--
-- 'rollbackOnDisable', 'autoTuneOptions_rollbackOnDisable' - Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
--
-- 'maintenanceSchedules', 'autoTuneOptions_maintenanceSchedules' - Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newAutoTuneOptions ::
  AutoTuneOptions
newAutoTuneOptions =
  AutoTuneOptions'
    { desiredState = Prelude.Nothing,
      rollbackOnDisable = Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing
    }

-- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
autoTuneOptions_desiredState :: Lens.Lens' AutoTuneOptions (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptions_desiredState = Lens.lens (\AutoTuneOptions' {desiredState} -> desiredState) (\s@AutoTuneOptions' {} a -> s {desiredState = a} :: AutoTuneOptions)

-- | Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
autoTuneOptions_rollbackOnDisable :: Lens.Lens' AutoTuneOptions (Prelude.Maybe RollbackOnDisable)
autoTuneOptions_rollbackOnDisable = Lens.lens (\AutoTuneOptions' {rollbackOnDisable} -> rollbackOnDisable) (\s@AutoTuneOptions' {} a -> s {rollbackOnDisable = a} :: AutoTuneOptions)

-- | Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneOptions_maintenanceSchedules :: Lens.Lens' AutoTuneOptions (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptions_maintenanceSchedules = Lens.lens (\AutoTuneOptions' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptions' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptions) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AutoTuneOptions where
  parseJSON =
    Prelude.withObject
      "AutoTuneOptions"
      ( \x ->
          AutoTuneOptions'
            Prelude.<$> (x Prelude..:? "DesiredState")
            Prelude.<*> (x Prelude..:? "RollbackOnDisable")
            Prelude.<*> ( x Prelude..:? "MaintenanceSchedules"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AutoTuneOptions

instance Prelude.NFData AutoTuneOptions

instance Prelude.ToJSON AutoTuneOptions where
  toJSON AutoTuneOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DesiredState" Prelude..=)
              Prelude.<$> desiredState,
            ("RollbackOnDisable" Prelude..=)
              Prelude.<$> rollbackOnDisable,
            ("MaintenanceSchedules" Prelude..=)
              Prelude.<$> maintenanceSchedules
          ]
      )
