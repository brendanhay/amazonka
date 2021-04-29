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
-- Module      : Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterAssociatedToSchedule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduleState

-- |
--
-- /See:/ 'newClusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { scheduleAssociationState :: Prelude.Maybe ScheduleState,
    clusterIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing
    }

-- |
clusterAssociatedToSchedule_scheduleAssociationState :: Lens.Lens' ClusterAssociatedToSchedule (Prelude.Maybe ScheduleState)
clusterAssociatedToSchedule_scheduleAssociationState = Lens.lens (\ClusterAssociatedToSchedule' {scheduleAssociationState} -> scheduleAssociationState) (\s@ClusterAssociatedToSchedule' {} a -> s {scheduleAssociationState = a} :: ClusterAssociatedToSchedule)

-- |
clusterAssociatedToSchedule_clusterIdentifier :: Lens.Lens' ClusterAssociatedToSchedule (Prelude.Maybe Prelude.Text)
clusterAssociatedToSchedule_clusterIdentifier = Lens.lens (\ClusterAssociatedToSchedule' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterAssociatedToSchedule' {} a -> s {clusterIdentifier = a} :: ClusterAssociatedToSchedule)

instance Prelude.FromXML ClusterAssociatedToSchedule where
  parseXML x =
    ClusterAssociatedToSchedule'
      Prelude.<$> (x Prelude..@? "ScheduleAssociationState")
      Prelude.<*> (x Prelude..@? "ClusterIdentifier")

instance Prelude.Hashable ClusterAssociatedToSchedule

instance Prelude.NFData ClusterAssociatedToSchedule
