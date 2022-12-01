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
-- Module      : Amazonka.Redshift.Types.ClusterParameterGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterParameterGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.ClusterParameterStatus

-- | Describes the status of a parameter group.
--
-- /See:/ 'newClusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { -- | The name of the cluster parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The list of parameter statuses.
    --
    -- For more information about parameters and parameter groups, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    clusterParameterStatusList :: Prelude.Maybe [ClusterParameterStatus],
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterParameterGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'clusterParameterGroupStatus_parameterGroupName' - The name of the cluster parameter group.
--
-- 'clusterParameterStatusList', 'clusterParameterGroupStatus_clusterParameterStatusList' - The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'parameterApplyStatus', 'clusterParameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
newClusterParameterGroupStatus ::
  ClusterParameterGroupStatus
newClusterParameterGroupStatus =
  ClusterParameterGroupStatus'
    { parameterGroupName =
        Prelude.Nothing,
      clusterParameterStatusList = Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing
    }

-- | The name of the cluster parameter group.
clusterParameterGroupStatus_parameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe Prelude.Text)
clusterParameterGroupStatus_parameterGroupName = Lens.lens (\ClusterParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ClusterParameterGroupStatus)

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
clusterParameterGroupStatus_clusterParameterStatusList :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe [ClusterParameterStatus])
clusterParameterGroupStatus_clusterParameterStatusList = Lens.lens (\ClusterParameterGroupStatus' {clusterParameterStatusList} -> clusterParameterStatusList) (\s@ClusterParameterGroupStatus' {} a -> s {clusterParameterStatusList = a} :: ClusterParameterGroupStatus) Prelude.. Lens.mapping Lens.coerced

-- | The status of parameter updates.
clusterParameterGroupStatus_parameterApplyStatus :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe Prelude.Text)
clusterParameterGroupStatus_parameterApplyStatus = Lens.lens (\ClusterParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ClusterParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: ClusterParameterGroupStatus)

instance Core.FromXML ClusterParameterGroupStatus where
  parseXML x =
    ClusterParameterGroupStatus'
      Prelude.<$> (x Core..@? "ParameterGroupName")
      Prelude.<*> ( x Core..@? "ClusterParameterStatusList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ParameterApplyStatus")

instance Prelude.Hashable ClusterParameterGroupStatus where
  hashWithSalt _salt ClusterParameterGroupStatus' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` clusterParameterStatusList
      `Prelude.hashWithSalt` parameterApplyStatus

instance Prelude.NFData ClusterParameterGroupStatus where
  rnf ClusterParameterGroupStatus' {..} =
    Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf clusterParameterStatusList
      `Prelude.seq` Prelude.rnf parameterApplyStatus
