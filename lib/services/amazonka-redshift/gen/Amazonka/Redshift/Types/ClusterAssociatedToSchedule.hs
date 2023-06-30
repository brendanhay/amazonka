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
-- Module      : Amazonka.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterAssociatedToSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.ScheduleState

-- |
--
-- /See:/ 'newClusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { clusterIdentifier :: Prelude.Maybe Prelude.Text,
    scheduleAssociationState :: Prelude.Maybe ScheduleState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterAssociatedToSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'clusterAssociatedToSchedule_clusterIdentifier' -
--
-- 'scheduleAssociationState', 'clusterAssociatedToSchedule_scheduleAssociationState' -
newClusterAssociatedToSchedule ::
  ClusterAssociatedToSchedule
newClusterAssociatedToSchedule =
  ClusterAssociatedToSchedule'
    { clusterIdentifier =
        Prelude.Nothing,
      scheduleAssociationState = Prelude.Nothing
    }

clusterAssociatedToSchedule_clusterIdentifier :: Lens.Lens' ClusterAssociatedToSchedule (Prelude.Maybe Prelude.Text)
clusterAssociatedToSchedule_clusterIdentifier = Lens.lens (\ClusterAssociatedToSchedule' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterAssociatedToSchedule' {} a -> s {clusterIdentifier = a} :: ClusterAssociatedToSchedule)

clusterAssociatedToSchedule_scheduleAssociationState :: Lens.Lens' ClusterAssociatedToSchedule (Prelude.Maybe ScheduleState)
clusterAssociatedToSchedule_scheduleAssociationState = Lens.lens (\ClusterAssociatedToSchedule' {scheduleAssociationState} -> scheduleAssociationState) (\s@ClusterAssociatedToSchedule' {} a -> s {scheduleAssociationState = a} :: ClusterAssociatedToSchedule)

instance Data.FromXML ClusterAssociatedToSchedule where
  parseXML x =
    ClusterAssociatedToSchedule'
      Prelude.<$> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "ScheduleAssociationState")

instance Prelude.Hashable ClusterAssociatedToSchedule where
  hashWithSalt _salt ClusterAssociatedToSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` scheduleAssociationState

instance Prelude.NFData ClusterAssociatedToSchedule where
  rnf ClusterAssociatedToSchedule' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf scheduleAssociationState
