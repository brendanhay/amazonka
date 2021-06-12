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
-- Module      : Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterAssociatedToSchedule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduleState

-- |
--
-- /See:/ 'newClusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { scheduleAssociationState :: Core.Maybe ScheduleState,
    clusterIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterAssociatedToSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleAssociationState', 'clusterAssociatedToSchedule_scheduleAssociationState' -
--
-- 'clusterIdentifier', 'clusterAssociatedToSchedule_clusterIdentifier' -
newClusterAssociatedToSchedule ::
  ClusterAssociatedToSchedule
newClusterAssociatedToSchedule =
  ClusterAssociatedToSchedule'
    { scheduleAssociationState =
        Core.Nothing,
      clusterIdentifier = Core.Nothing
    }

-- |
clusterAssociatedToSchedule_scheduleAssociationState :: Lens.Lens' ClusterAssociatedToSchedule (Core.Maybe ScheduleState)
clusterAssociatedToSchedule_scheduleAssociationState = Lens.lens (\ClusterAssociatedToSchedule' {scheduleAssociationState} -> scheduleAssociationState) (\s@ClusterAssociatedToSchedule' {} a -> s {scheduleAssociationState = a} :: ClusterAssociatedToSchedule)

-- |
clusterAssociatedToSchedule_clusterIdentifier :: Lens.Lens' ClusterAssociatedToSchedule (Core.Maybe Core.Text)
clusterAssociatedToSchedule_clusterIdentifier = Lens.lens (\ClusterAssociatedToSchedule' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterAssociatedToSchedule' {} a -> s {clusterIdentifier = a} :: ClusterAssociatedToSchedule)

instance Core.FromXML ClusterAssociatedToSchedule where
  parseXML x =
    ClusterAssociatedToSchedule'
      Core.<$> (x Core..@? "ScheduleAssociationState")
      Core.<*> (x Core..@? "ClusterIdentifier")

instance Core.Hashable ClusterAssociatedToSchedule

instance Core.NFData ClusterAssociatedToSchedule
