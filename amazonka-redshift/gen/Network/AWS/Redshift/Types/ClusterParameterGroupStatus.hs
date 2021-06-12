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
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterParameterStatus

-- | Describes the status of a parameter group.
--
-- /See:/ 'newClusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { -- | The list of parameter statuses.
    --
    -- For more information about parameters and parameter groups, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    clusterParameterStatusList :: Core.Maybe [ClusterParameterStatus],
    -- | The name of the cluster parameter group.
    parameterGroupName :: Core.Maybe Core.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterParameterGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterParameterStatusList', 'clusterParameterGroupStatus_clusterParameterStatusList' - The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'parameterGroupName', 'clusterParameterGroupStatus_parameterGroupName' - The name of the cluster parameter group.
--
-- 'parameterApplyStatus', 'clusterParameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
newClusterParameterGroupStatus ::
  ClusterParameterGroupStatus
newClusterParameterGroupStatus =
  ClusterParameterGroupStatus'
    { clusterParameterStatusList =
        Core.Nothing,
      parameterGroupName = Core.Nothing,
      parameterApplyStatus = Core.Nothing
    }

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
clusterParameterGroupStatus_clusterParameterStatusList :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe [ClusterParameterStatus])
clusterParameterGroupStatus_clusterParameterStatusList = Lens.lens (\ClusterParameterGroupStatus' {clusterParameterStatusList} -> clusterParameterStatusList) (\s@ClusterParameterGroupStatus' {} a -> s {clusterParameterStatusList = a} :: ClusterParameterGroupStatus) Core.. Lens.mapping Lens._Coerce

-- | The name of the cluster parameter group.
clusterParameterGroupStatus_parameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe Core.Text)
clusterParameterGroupStatus_parameterGroupName = Lens.lens (\ClusterParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ClusterParameterGroupStatus)

-- | The status of parameter updates.
clusterParameterGroupStatus_parameterApplyStatus :: Lens.Lens' ClusterParameterGroupStatus (Core.Maybe Core.Text)
clusterParameterGroupStatus_parameterApplyStatus = Lens.lens (\ClusterParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ClusterParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: ClusterParameterGroupStatus)

instance Core.FromXML ClusterParameterGroupStatus where
  parseXML x =
    ClusterParameterGroupStatus'
      Core.<$> ( x Core..@? "ClusterParameterStatusList"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ParameterGroupName")
      Core.<*> (x Core..@? "ParameterApplyStatus")

instance Core.Hashable ClusterParameterGroupStatus

instance Core.NFData ClusterParameterGroupStatus
