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
-- Module      : Network.AWS.RDS.Types.DBProxyTargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTargetGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo

-- | Represents a set of RDS DB instances, Aurora DB clusters, or both that a
-- proxy can connect to. Currently, each target group is associated with
-- exactly one RDS DB instance or Aurora DB cluster.
--
-- This data type is used as a response element in the
-- @DescribeDBProxyTargetGroups@ action.
--
-- /See:/ 'newDBProxyTargetGroup' smart constructor.
data DBProxyTargetGroup = DBProxyTargetGroup'
  { -- | The current status of this target group. A status of @available@ means
    -- the target group is correctly associated with a database. Other values
    -- indicate that you must wait for the target group to be ready, or take
    -- some action to resolve an issue.
    status :: Core.Maybe Core.Text,
    -- | The date and time when the target group was first created.
    createdDate :: Core.Maybe Core.ISO8601,
    -- | Whether this target group is the first one used for connection requests
    -- by the associated proxy. Because each proxy is currently associated with
    -- a single target group, currently this setting is always @true@.
    isDefault :: Core.Maybe Core.Bool,
    -- | The identifier for the target group. This name must be unique for all
    -- target groups owned by your AWS account in the specified AWS Region.
    targetGroupName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) representing the target group.
    targetGroupArn :: Core.Maybe Core.Text,
    -- | The settings that determine the size and behavior of the connection pool
    -- for the target group.
    connectionPoolConfig :: Core.Maybe ConnectionPoolConfigurationInfo,
    -- | The date and time when the target group was last updated.
    updatedDate :: Core.Maybe Core.ISO8601,
    -- | The identifier for the RDS proxy associated with this target group.
    dbProxyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBProxyTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbProxyTargetGroup_status' - The current status of this target group. A status of @available@ means
-- the target group is correctly associated with a database. Other values
-- indicate that you must wait for the target group to be ready, or take
-- some action to resolve an issue.
--
-- 'createdDate', 'dbProxyTargetGroup_createdDate' - The date and time when the target group was first created.
--
-- 'isDefault', 'dbProxyTargetGroup_isDefault' - Whether this target group is the first one used for connection requests
-- by the associated proxy. Because each proxy is currently associated with
-- a single target group, currently this setting is always @true@.
--
-- 'targetGroupName', 'dbProxyTargetGroup_targetGroupName' - The identifier for the target group. This name must be unique for all
-- target groups owned by your AWS account in the specified AWS Region.
--
-- 'targetGroupArn', 'dbProxyTargetGroup_targetGroupArn' - The Amazon Resource Name (ARN) representing the target group.
--
-- 'connectionPoolConfig', 'dbProxyTargetGroup_connectionPoolConfig' - The settings that determine the size and behavior of the connection pool
-- for the target group.
--
-- 'updatedDate', 'dbProxyTargetGroup_updatedDate' - The date and time when the target group was last updated.
--
-- 'dbProxyName', 'dbProxyTargetGroup_dbProxyName' - The identifier for the RDS proxy associated with this target group.
newDBProxyTargetGroup ::
  DBProxyTargetGroup
newDBProxyTargetGroup =
  DBProxyTargetGroup'
    { status = Core.Nothing,
      createdDate = Core.Nothing,
      isDefault = Core.Nothing,
      targetGroupName = Core.Nothing,
      targetGroupArn = Core.Nothing,
      connectionPoolConfig = Core.Nothing,
      updatedDate = Core.Nothing,
      dbProxyName = Core.Nothing
    }

-- | The current status of this target group. A status of @available@ means
-- the target group is correctly associated with a database. Other values
-- indicate that you must wait for the target group to be ready, or take
-- some action to resolve an issue.
dbProxyTargetGroup_status :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbProxyTargetGroup_status = Lens.lens (\DBProxyTargetGroup' {status} -> status) (\s@DBProxyTargetGroup' {} a -> s {status = a} :: DBProxyTargetGroup)

-- | The date and time when the target group was first created.
dbProxyTargetGroup_createdDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbProxyTargetGroup_createdDate = Lens.lens (\DBProxyTargetGroup' {createdDate} -> createdDate) (\s@DBProxyTargetGroup' {} a -> s {createdDate = a} :: DBProxyTargetGroup) Core.. Lens.mapping Core._Time

-- | Whether this target group is the first one used for connection requests
-- by the associated proxy. Because each proxy is currently associated with
-- a single target group, currently this setting is always @true@.
dbProxyTargetGroup_isDefault :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Bool)
dbProxyTargetGroup_isDefault = Lens.lens (\DBProxyTargetGroup' {isDefault} -> isDefault) (\s@DBProxyTargetGroup' {} a -> s {isDefault = a} :: DBProxyTargetGroup)

-- | The identifier for the target group. This name must be unique for all
-- target groups owned by your AWS account in the specified AWS Region.
dbProxyTargetGroup_targetGroupName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbProxyTargetGroup_targetGroupName = Lens.lens (\DBProxyTargetGroup' {targetGroupName} -> targetGroupName) (\s@DBProxyTargetGroup' {} a -> s {targetGroupName = a} :: DBProxyTargetGroup)

-- | The Amazon Resource Name (ARN) representing the target group.
dbProxyTargetGroup_targetGroupArn :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbProxyTargetGroup_targetGroupArn = Lens.lens (\DBProxyTargetGroup' {targetGroupArn} -> targetGroupArn) (\s@DBProxyTargetGroup' {} a -> s {targetGroupArn = a} :: DBProxyTargetGroup)

-- | The settings that determine the size and behavior of the connection pool
-- for the target group.
dbProxyTargetGroup_connectionPoolConfig :: Lens.Lens' DBProxyTargetGroup (Core.Maybe ConnectionPoolConfigurationInfo)
dbProxyTargetGroup_connectionPoolConfig = Lens.lens (\DBProxyTargetGroup' {connectionPoolConfig} -> connectionPoolConfig) (\s@DBProxyTargetGroup' {} a -> s {connectionPoolConfig = a} :: DBProxyTargetGroup)

-- | The date and time when the target group was last updated.
dbProxyTargetGroup_updatedDate :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.UTCTime)
dbProxyTargetGroup_updatedDate = Lens.lens (\DBProxyTargetGroup' {updatedDate} -> updatedDate) (\s@DBProxyTargetGroup' {} a -> s {updatedDate = a} :: DBProxyTargetGroup) Core.. Lens.mapping Core._Time

-- | The identifier for the RDS proxy associated with this target group.
dbProxyTargetGroup_dbProxyName :: Lens.Lens' DBProxyTargetGroup (Core.Maybe Core.Text)
dbProxyTargetGroup_dbProxyName = Lens.lens (\DBProxyTargetGroup' {dbProxyName} -> dbProxyName) (\s@DBProxyTargetGroup' {} a -> s {dbProxyName = a} :: DBProxyTargetGroup)

instance Core.FromXML DBProxyTargetGroup where
  parseXML x =
    DBProxyTargetGroup'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "CreatedDate")
      Core.<*> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "TargetGroupName")
      Core.<*> (x Core..@? "TargetGroupArn")
      Core.<*> (x Core..@? "ConnectionPoolConfig")
      Core.<*> (x Core..@? "UpdatedDate")
      Core.<*> (x Core..@? "DBProxyName")

instance Core.Hashable DBProxyTargetGroup

instance Core.NFData DBProxyTargetGroup
