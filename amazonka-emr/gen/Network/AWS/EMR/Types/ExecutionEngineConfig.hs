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
-- Module      : Network.AWS.EMR.Types.ExecutionEngineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ExecutionEngineConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ExecutionEngineType
import qualified Network.AWS.Lens as Lens

-- | Specifies the execution engine (cluster) to run the notebook and perform
-- the notebook execution, for example, an EMR cluster.
--
-- /See:/ 'newExecutionEngineConfig' smart constructor.
data ExecutionEngineConfig = ExecutionEngineConfig'
  { -- | An optional unique ID of an EC2 security group to associate with the
    -- master instance of the EMR cluster for this notebook execution. For more
    -- information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
    -- in the /EMR Management Guide/.
    masterInstanceSecurityGroupId :: Core.Maybe Core.Text,
    -- | The type of execution engine. A value of @EMR@ specifies an EMR cluster.
    type' :: Core.Maybe ExecutionEngineType,
    -- | The unique identifier of the execution engine. For an EMR cluster, this
    -- is the cluster ID.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecutionEngineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterInstanceSecurityGroupId', 'executionEngineConfig_masterInstanceSecurityGroupId' - An optional unique ID of an EC2 security group to associate with the
-- master instance of the EMR cluster for this notebook execution. For more
-- information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
-- in the /EMR Management Guide/.
--
-- 'type'', 'executionEngineConfig_type' - The type of execution engine. A value of @EMR@ specifies an EMR cluster.
--
-- 'id', 'executionEngineConfig_id' - The unique identifier of the execution engine. For an EMR cluster, this
-- is the cluster ID.
newExecutionEngineConfig ::
  -- | 'id'
  Core.Text ->
  ExecutionEngineConfig
newExecutionEngineConfig pId_ =
  ExecutionEngineConfig'
    { masterInstanceSecurityGroupId =
        Core.Nothing,
      type' = Core.Nothing,
      id = pId_
    }

-- | An optional unique ID of an EC2 security group to associate with the
-- master instance of the EMR cluster for this notebook execution. For more
-- information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks>
-- in the /EMR Management Guide/.
executionEngineConfig_masterInstanceSecurityGroupId :: Lens.Lens' ExecutionEngineConfig (Core.Maybe Core.Text)
executionEngineConfig_masterInstanceSecurityGroupId = Lens.lens (\ExecutionEngineConfig' {masterInstanceSecurityGroupId} -> masterInstanceSecurityGroupId) (\s@ExecutionEngineConfig' {} a -> s {masterInstanceSecurityGroupId = a} :: ExecutionEngineConfig)

-- | The type of execution engine. A value of @EMR@ specifies an EMR cluster.
executionEngineConfig_type :: Lens.Lens' ExecutionEngineConfig (Core.Maybe ExecutionEngineType)
executionEngineConfig_type = Lens.lens (\ExecutionEngineConfig' {type'} -> type') (\s@ExecutionEngineConfig' {} a -> s {type' = a} :: ExecutionEngineConfig)

-- | The unique identifier of the execution engine. For an EMR cluster, this
-- is the cluster ID.
executionEngineConfig_id :: Lens.Lens' ExecutionEngineConfig Core.Text
executionEngineConfig_id = Lens.lens (\ExecutionEngineConfig' {id} -> id) (\s@ExecutionEngineConfig' {} a -> s {id = a} :: ExecutionEngineConfig)

instance Core.FromJSON ExecutionEngineConfig where
  parseJSON =
    Core.withObject
      "ExecutionEngineConfig"
      ( \x ->
          ExecutionEngineConfig'
            Core.<$> (x Core..:? "MasterInstanceSecurityGroupId")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..: "Id")
      )

instance Core.Hashable ExecutionEngineConfig

instance Core.NFData ExecutionEngineConfig

instance Core.ToJSON ExecutionEngineConfig where
  toJSON ExecutionEngineConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MasterInstanceSecurityGroupId" Core..=)
              Core.<$> masterInstanceSecurityGroupId,
            ("Type" Core..=) Core.<$> type',
            Core.Just ("Id" Core..= id)
          ]
      )
