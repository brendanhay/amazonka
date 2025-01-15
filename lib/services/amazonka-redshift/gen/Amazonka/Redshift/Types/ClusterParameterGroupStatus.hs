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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterParameterGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.ClusterParameterStatus

-- | Describes the status of a parameter group.
--
-- /See:/ 'newClusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { -- | The list of parameter statuses.
    --
    -- For more information about parameters and parameter groups, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    clusterParameterStatusList :: Prelude.Maybe [ClusterParameterStatus],
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text
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
-- 'clusterParameterStatusList', 'clusterParameterGroupStatus_clusterParameterStatusList' - The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'parameterApplyStatus', 'clusterParameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
--
-- 'parameterGroupName', 'clusterParameterGroupStatus_parameterGroupName' - The name of the cluster parameter group.
newClusterParameterGroupStatus ::
  ClusterParameterGroupStatus
newClusterParameterGroupStatus =
  ClusterParameterGroupStatus'
    { clusterParameterStatusList =
        Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing
    }

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

-- | The name of the cluster parameter group.
clusterParameterGroupStatus_parameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe Prelude.Text)
clusterParameterGroupStatus_parameterGroupName = Lens.lens (\ClusterParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ClusterParameterGroupStatus)

instance Data.FromXML ClusterParameterGroupStatus where
  parseXML x =
    ClusterParameterGroupStatus'
      Prelude.<$> ( x
                      Data..@? "ClusterParameterStatusList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "ParameterApplyStatus")
      Prelude.<*> (x Data..@? "ParameterGroupName")

instance Prelude.Hashable ClusterParameterGroupStatus where
  hashWithSalt _salt ClusterParameterGroupStatus' {..} =
    _salt
      `Prelude.hashWithSalt` clusterParameterStatusList
      `Prelude.hashWithSalt` parameterApplyStatus
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData ClusterParameterGroupStatus where
  rnf ClusterParameterGroupStatus' {..} =
    Prelude.rnf clusterParameterStatusList `Prelude.seq`
      Prelude.rnf parameterApplyStatus `Prelude.seq`
        Prelude.rnf parameterGroupName
