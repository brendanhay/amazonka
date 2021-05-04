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
-- Module      : Network.AWS.RDS.Types.FailoverState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.FailoverState where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.FailoverStatus

-- | Contains the state of scheduled or in-process failover operations on an
-- Aurora global database (GlobalCluster). This Data type is empty unless a
-- failover operation is scheduled or is currently underway on the Aurora
-- global database.
--
-- /See:/ 'newFailoverState' smart constructor.
data FailoverState = FailoverState'
  { -- | The current status of the Aurora global database (GlobalCluster).
    -- Possible values are as follows:
    --
    -- -   pending – A request to fail over the Aurora global database
    --     (GlobalCluster) has been received by the service. The
    --     @GlobalCluster@\'s primary DB cluster and the specified secondary DB
    --     cluster are being verified before the failover process can start.
    --
    -- -   failing-over – This status covers the range of Aurora internal
    --     operations that take place during the failover process, such as
    --     demoting the primary Aurora DB cluster, promoting the secondary
    --     Aurora DB, and synchronizing replicas.
    --
    -- -   cancelling – The request to fail over the Aurora global database
    --     (GlobalCluster) was cancelled and the primary Aurora DB cluster and
    --     the selected secondary Aurora DB cluster are returning to their
    --     previous states.
    status :: Prelude.Maybe FailoverStatus,
    -- | The Amazon Resource Name (ARN) of the Aurora DB cluster that is
    -- currently being promoted, and which is associated with this state.
    toDbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Aurora DB cluster that is
    -- currently being demoted, and which is associated with this state.
    fromDbClusterArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailoverState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'failoverState_status' - The current status of the Aurora global database (GlobalCluster).
-- Possible values are as follows:
--
-- -   pending – A request to fail over the Aurora global database
--     (GlobalCluster) has been received by the service. The
--     @GlobalCluster@\'s primary DB cluster and the specified secondary DB
--     cluster are being verified before the failover process can start.
--
-- -   failing-over – This status covers the range of Aurora internal
--     operations that take place during the failover process, such as
--     demoting the primary Aurora DB cluster, promoting the secondary
--     Aurora DB, and synchronizing replicas.
--
-- -   cancelling – The request to fail over the Aurora global database
--     (GlobalCluster) was cancelled and the primary Aurora DB cluster and
--     the selected secondary Aurora DB cluster are returning to their
--     previous states.
--
-- 'toDbClusterArn', 'failoverState_toDbClusterArn' - The Amazon Resource Name (ARN) of the Aurora DB cluster that is
-- currently being promoted, and which is associated with this state.
--
-- 'fromDbClusterArn', 'failoverState_fromDbClusterArn' - The Amazon Resource Name (ARN) of the Aurora DB cluster that is
-- currently being demoted, and which is associated with this state.
newFailoverState ::
  FailoverState
newFailoverState =
  FailoverState'
    { status = Prelude.Nothing,
      toDbClusterArn = Prelude.Nothing,
      fromDbClusterArn = Prelude.Nothing
    }

-- | The current status of the Aurora global database (GlobalCluster).
-- Possible values are as follows:
--
-- -   pending – A request to fail over the Aurora global database
--     (GlobalCluster) has been received by the service. The
--     @GlobalCluster@\'s primary DB cluster and the specified secondary DB
--     cluster are being verified before the failover process can start.
--
-- -   failing-over – This status covers the range of Aurora internal
--     operations that take place during the failover process, such as
--     demoting the primary Aurora DB cluster, promoting the secondary
--     Aurora DB, and synchronizing replicas.
--
-- -   cancelling – The request to fail over the Aurora global database
--     (GlobalCluster) was cancelled and the primary Aurora DB cluster and
--     the selected secondary Aurora DB cluster are returning to their
--     previous states.
failoverState_status :: Lens.Lens' FailoverState (Prelude.Maybe FailoverStatus)
failoverState_status = Lens.lens (\FailoverState' {status} -> status) (\s@FailoverState' {} a -> s {status = a} :: FailoverState)

-- | The Amazon Resource Name (ARN) of the Aurora DB cluster that is
-- currently being promoted, and which is associated with this state.
failoverState_toDbClusterArn :: Lens.Lens' FailoverState (Prelude.Maybe Prelude.Text)
failoverState_toDbClusterArn = Lens.lens (\FailoverState' {toDbClusterArn} -> toDbClusterArn) (\s@FailoverState' {} a -> s {toDbClusterArn = a} :: FailoverState)

-- | The Amazon Resource Name (ARN) of the Aurora DB cluster that is
-- currently being demoted, and which is associated with this state.
failoverState_fromDbClusterArn :: Lens.Lens' FailoverState (Prelude.Maybe Prelude.Text)
failoverState_fromDbClusterArn = Lens.lens (\FailoverState' {fromDbClusterArn} -> fromDbClusterArn) (\s@FailoverState' {} a -> s {fromDbClusterArn = a} :: FailoverState)

instance Prelude.FromXML FailoverState where
  parseXML x =
    FailoverState'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "ToDbClusterArn")
      Prelude.<*> (x Prelude..@? "FromDbClusterArn")

instance Prelude.Hashable FailoverState

instance Prelude.NFData FailoverState
