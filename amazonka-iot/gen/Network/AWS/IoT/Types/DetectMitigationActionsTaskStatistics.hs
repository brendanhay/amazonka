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
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The statistics of a mitigation action task.
--
-- /See:/ 'newDetectMitigationActionsTaskStatistics' smart constructor.
data DetectMitigationActionsTaskStatistics = DetectMitigationActionsTaskStatistics'
  { -- | The actions that failed.
    actionsFailed :: Prelude.Maybe Prelude.Integer,
    -- | The actions that were skipped.
    actionsSkipped :: Prelude.Maybe Prelude.Integer,
    -- | The actions that were performed.
    actionsExecuted :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectMitigationActionsTaskStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionsFailed', 'detectMitigationActionsTaskStatistics_actionsFailed' - The actions that failed.
--
-- 'actionsSkipped', 'detectMitigationActionsTaskStatistics_actionsSkipped' - The actions that were skipped.
--
-- 'actionsExecuted', 'detectMitigationActionsTaskStatistics_actionsExecuted' - The actions that were performed.
newDetectMitigationActionsTaskStatistics ::
  DetectMitigationActionsTaskStatistics
newDetectMitigationActionsTaskStatistics =
  DetectMitigationActionsTaskStatistics'
    { actionsFailed =
        Prelude.Nothing,
      actionsSkipped = Prelude.Nothing,
      actionsExecuted = Prelude.Nothing
    }

-- | The actions that failed.
detectMitigationActionsTaskStatistics_actionsFailed :: Lens.Lens' DetectMitigationActionsTaskStatistics (Prelude.Maybe Prelude.Integer)
detectMitigationActionsTaskStatistics_actionsFailed = Lens.lens (\DetectMitigationActionsTaskStatistics' {actionsFailed} -> actionsFailed) (\s@DetectMitigationActionsTaskStatistics' {} a -> s {actionsFailed = a} :: DetectMitigationActionsTaskStatistics)

-- | The actions that were skipped.
detectMitigationActionsTaskStatistics_actionsSkipped :: Lens.Lens' DetectMitigationActionsTaskStatistics (Prelude.Maybe Prelude.Integer)
detectMitigationActionsTaskStatistics_actionsSkipped = Lens.lens (\DetectMitigationActionsTaskStatistics' {actionsSkipped} -> actionsSkipped) (\s@DetectMitigationActionsTaskStatistics' {} a -> s {actionsSkipped = a} :: DetectMitigationActionsTaskStatistics)

-- | The actions that were performed.
detectMitigationActionsTaskStatistics_actionsExecuted :: Lens.Lens' DetectMitigationActionsTaskStatistics (Prelude.Maybe Prelude.Integer)
detectMitigationActionsTaskStatistics_actionsExecuted = Lens.lens (\DetectMitigationActionsTaskStatistics' {actionsExecuted} -> actionsExecuted) (\s@DetectMitigationActionsTaskStatistics' {} a -> s {actionsExecuted = a} :: DetectMitigationActionsTaskStatistics)

instance
  Prelude.FromJSON
    DetectMitigationActionsTaskStatistics
  where
  parseJSON =
    Prelude.withObject
      "DetectMitigationActionsTaskStatistics"
      ( \x ->
          DetectMitigationActionsTaskStatistics'
            Prelude.<$> (x Prelude..:? "actionsFailed")
            Prelude.<*> (x Prelude..:? "actionsSkipped")
            Prelude.<*> (x Prelude..:? "actionsExecuted")
      )

instance
  Prelude.Hashable
    DetectMitigationActionsTaskStatistics

instance
  Prelude.NFData
    DetectMitigationActionsTaskStatistics
