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
-- Module      : Network.AWS.RDS.Types.DBProxyTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTarget where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.TargetType

-- | Contains the details for an RDS Proxy target. It represents an RDS DB
-- instance or Aurora DB cluster that the proxy can connect to. One or more
-- targets are associated with an RDS Proxy target group.
--
-- This data type is used as a response element in the
-- @DescribeDBProxyTargets@ action.
--
-- /See:/ 'newDBProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { -- | The DB cluster identifier when the target represents an Aurora DB
    -- cluster. This field is blank when the target represents an RDS DB
    -- instance.
    trackedClusterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier representing the target. It can be the instance
    -- identifier for an RDS DB instance, or the cluster identifier for an
    -- Aurora DB cluster.
    rdsResourceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
    -- cluster.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The port that the RDS Proxy uses to connect to the target RDS DB
    -- instance or Aurora DB cluster.
    port :: Prelude.Maybe Prelude.Int,
    -- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Specifies the kind of database, such as an RDS DB instance or an Aurora
    -- DB cluster, that the target represents.
    type' :: Prelude.Maybe TargetType,
    -- | Information about the connection health of the RDS Proxy target.
    targetHealth :: Prelude.Maybe TargetHealth
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DBProxyTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackedClusterId', 'dbProxyTarget_trackedClusterId' - The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
--
-- 'rdsResourceId', 'dbProxyTarget_rdsResourceId' - The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
--
-- 'targetArn', 'dbProxyTarget_targetArn' - The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
--
-- 'port', 'dbProxyTarget_port' - The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
--
-- 'endpoint', 'dbProxyTarget_endpoint' - The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- 'type'', 'dbProxyTarget_type' - Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
--
-- 'targetHealth', 'dbProxyTarget_targetHealth' - Information about the connection health of the RDS Proxy target.
newDBProxyTarget ::
  DBProxyTarget
newDBProxyTarget =
  DBProxyTarget'
    { trackedClusterId = Prelude.Nothing,
      rdsResourceId = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      port = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      type' = Prelude.Nothing,
      targetHealth = Prelude.Nothing
    }

-- | The DB cluster identifier when the target represents an Aurora DB
-- cluster. This field is blank when the target represents an RDS DB
-- instance.
dbProxyTarget_trackedClusterId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_trackedClusterId = Lens.lens (\DBProxyTarget' {trackedClusterId} -> trackedClusterId) (\s@DBProxyTarget' {} a -> s {trackedClusterId = a} :: DBProxyTarget)

-- | The identifier representing the target. It can be the instance
-- identifier for an RDS DB instance, or the cluster identifier for an
-- Aurora DB cluster.
dbProxyTarget_rdsResourceId :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_rdsResourceId = Lens.lens (\DBProxyTarget' {rdsResourceId} -> rdsResourceId) (\s@DBProxyTarget' {} a -> s {rdsResourceId = a} :: DBProxyTarget)

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB
-- cluster.
dbProxyTarget_targetArn :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_targetArn = Lens.lens (\DBProxyTarget' {targetArn} -> targetArn) (\s@DBProxyTarget' {} a -> s {targetArn = a} :: DBProxyTarget)

-- | The port that the RDS Proxy uses to connect to the target RDS DB
-- instance or Aurora DB cluster.
dbProxyTarget_port :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Int)
dbProxyTarget_port = Lens.lens (\DBProxyTarget' {port} -> port) (\s@DBProxyTarget' {} a -> s {port = a} :: DBProxyTarget)

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
dbProxyTarget_endpoint :: Lens.Lens' DBProxyTarget (Prelude.Maybe Prelude.Text)
dbProxyTarget_endpoint = Lens.lens (\DBProxyTarget' {endpoint} -> endpoint) (\s@DBProxyTarget' {} a -> s {endpoint = a} :: DBProxyTarget)

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora
-- DB cluster, that the target represents.
dbProxyTarget_type :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetType)
dbProxyTarget_type = Lens.lens (\DBProxyTarget' {type'} -> type') (\s@DBProxyTarget' {} a -> s {type' = a} :: DBProxyTarget)

-- | Information about the connection health of the RDS Proxy target.
dbProxyTarget_targetHealth :: Lens.Lens' DBProxyTarget (Prelude.Maybe TargetHealth)
dbProxyTarget_targetHealth = Lens.lens (\DBProxyTarget' {targetHealth} -> targetHealth) (\s@DBProxyTarget' {} a -> s {targetHealth = a} :: DBProxyTarget)

instance Prelude.FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      Prelude.<$> (x Prelude..@? "TrackedClusterId")
      Prelude.<*> (x Prelude..@? "RdsResourceId")
      Prelude.<*> (x Prelude..@? "TargetArn")
      Prelude.<*> (x Prelude..@? "Port")
      Prelude.<*> (x Prelude..@? "Endpoint")
      Prelude.<*> (x Prelude..@? "Type")
      Prelude.<*> (x Prelude..@? "TargetHealth")

instance Prelude.Hashable DBProxyTarget

instance Prelude.NFData DBProxyTarget
