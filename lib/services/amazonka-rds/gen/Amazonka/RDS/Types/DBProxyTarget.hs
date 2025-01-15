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
-- Module      : Amazonka.RDS.Types.DBProxyTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxyTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.TargetHealth
import Amazonka.RDS.Types.TargetRole
import Amazonka.RDS.Types.TargetType

-- | Contains the details for an RDS Proxy target. It represents an RDS DB
-- instance or Aurora DB cluster that the proxy can connect to. One or more
-- targets are associated with an RDS Proxy target group.
--
-- This data type is used as a response element in the
-- @DescribeDBProxyTargets@ action.
--
-- /See:/ 'newDBProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { -- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The port that the RDS Proxy uses to connect to the target RDS DB
    -- instance or Aurora DB cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | The identifier representing the target. It can be the instance
    -- identifier for an RDS DB instance, or the cluster identifier for an
    -- Aurora DB cluster.
    rdsResourceId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the target of the proxy can be used for
    -- read\/write or read-only operations.
    role' :: Prelude.Maybe TargetRole,
    -- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
    -- cluster.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the connection health of the RDS Proxy target.
    targetHealth :: Prelude.Maybe TargetHealth,
    -- | The DB cluster identifier when the target represents an Aurora DB
    -- cluster. This field is blank when the target represents an RDS DB
    -- instance.
    trackedClusterId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the kind of database, such as an RDS DB instance or an Aurora
    -- DB cluster, that the target represents.
    type' :: Prelude.Maybe TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBProxyTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'dbProxyTarget_endpoint' - The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- 'port', 'dbProxyTarget_port' - The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
--
-- 'rdsResourceId', 'dbProxyTarget_rdsResourceId' - The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
--
-- 'role'', 'dbProxyTarget_role' - A value that indicates whether the target of the proxy can be used for
-- read\/write or read-only operations.
--
-- 'targetArn', 'dbProxyTarget_targetArn' - The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
--
-- 'targetHealth', 'dbProxyTarget_targetHealth' - Information about the connection health of the RDS Proxy target.
--
-- 'trackedClusterId', 'dbProxyTarget_trackedClusterId' - The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
--
-- 'type'', 'dbProxyTarget_type' - Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
newDBProxyTarget ::
  DBProxyTarget
newDBProxyTarget =
  DBProxyTarget'
    { endpoint = Prelude.Nothing,
      port = Prelude.Nothing,
      rdsResourceId = Prelude.Nothing,
      role' = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      targetHealth = Prelude.Nothing,
      trackedClusterId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
dbProxyTarget_endpoint :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_endpoint = Lens.lens (\DBProxyTarget' {endpoint} -> endpoint) (\s@DBProxyTarget' {} a -> s {endpoint = a} :: DBProxyTarget)

-- | The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
dbProxyTarget_port :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Int)
dbProxyTarget_port = Lens.lens (\DBProxyTarget' {port} -> port) (\s@DBProxyTarget' {} a -> s {port = a} :: DBProxyTarget)

-- | The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
dbProxyTarget_rdsResourceId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_rdsResourceId = Lens.lens (\DBProxyTarget' {rdsResourceId} -> rdsResourceId) (\s@DBProxyTarget' {} a -> s {rdsResourceId = a} :: DBProxyTarget)

-- | A value that indicates whether the target of the proxy can be used for
-- read\/write or read-only operations.
dbProxyTarget_role :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetRole)
dbProxyTarget_role = Lens.lens (\DBProxyTarget' {role'} -> role') (\s@DBProxyTarget' {} a -> s {role' = a} :: DBProxyTarget)

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
dbProxyTarget_targetArn :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_targetArn = Lens.lens (\DBProxyTarget' {targetArn} -> targetArn) (\s@DBProxyTarget' {} a -> s {targetArn = a} :: DBProxyTarget)

-- | Information about the connection health of the RDS Proxy target.
dbProxyTarget_targetHealth :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetHealth)
dbProxyTarget_targetHealth = Lens.lens (\DBProxyTarget' {targetHealth} -> targetHealth) (\s@DBProxyTarget' {} a -> s {targetHealth = a} :: DBProxyTarget)

-- | The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
dbProxyTarget_trackedClusterId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_trackedClusterId = Lens.lens (\DBProxyTarget' {trackedClusterId} -> trackedClusterId) (\s@DBProxyTarget' {} a -> s {trackedClusterId = a} :: DBProxyTarget)

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
dbProxyTarget_type :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetType)
dbProxyTarget_type = Lens.lens (\DBProxyTarget' {type'} -> type') (\s@DBProxyTarget' {} a -> s {type' = a} :: DBProxyTarget)

instance Data.FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      Prelude.<$> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "RdsResourceId")
      Prelude.<*> (x Data..@? "Role")
      Prelude.<*> (x Data..@? "TargetArn")
      Prelude.<*> (x Data..@? "TargetHealth")
      Prelude.<*> (x Data..@? "TrackedClusterId")
      Prelude.<*> (x Data..@? "Type")

instance Prelude.Hashable DBProxyTarget where
  hashWithSalt _salt DBProxyTarget' {..} =
    _salt
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` rdsResourceId
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` targetHealth
      `Prelude.hashWithSalt` trackedClusterId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DBProxyTarget where
  rnf DBProxyTarget' {..} =
    Prelude.rnf endpoint `Prelude.seq`
      Prelude.rnf port `Prelude.seq`
        Prelude.rnf rdsResourceId `Prelude.seq`
          Prelude.rnf role' `Prelude.seq`
            Prelude.rnf targetArn `Prelude.seq`
              Prelude.rnf targetHealth `Prelude.seq`
                Prelude.rnf trackedClusterId `Prelude.seq`
                  Prelude.rnf type'
