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
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clusterParameterStatusList :: Prelude.Maybe [ClusterParameterStatus],
    -- | The name of the cluster parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing
    }

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
clusterParameterGroupStatus_clusterParameterStatusList :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe [ClusterParameterStatus])
clusterParameterGroupStatus_clusterParameterStatusList = Lens.lens (\ClusterParameterGroupStatus' {clusterParameterStatusList} -> clusterParameterStatusList) (\s@ClusterParameterGroupStatus' {} a -> s {clusterParameterStatusList = a} :: ClusterParameterGroupStatus) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the cluster parameter group.
clusterParameterGroupStatus_parameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe Prelude.Text)
clusterParameterGroupStatus_parameterGroupName = Lens.lens (\ClusterParameterGroupStatus' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroupStatus' {} a -> s {parameterGroupName = a} :: ClusterParameterGroupStatus)

-- | The status of parameter updates.
clusterParameterGroupStatus_parameterApplyStatus :: Lens.Lens' ClusterParameterGroupStatus (Prelude.Maybe Prelude.Text)
clusterParameterGroupStatus_parameterApplyStatus = Lens.lens (\ClusterParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ClusterParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: ClusterParameterGroupStatus)

instance Prelude.FromXML ClusterParameterGroupStatus where
  parseXML x =
    ClusterParameterGroupStatus'
      Prelude.<$> ( x Prelude..@? "ClusterParameterStatusList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ParameterGroupName")
      Prelude.<*> (x Prelude..@? "ParameterApplyStatus")

instance Prelude.Hashable ClusterParameterGroupStatus

instance Prelude.NFData ClusterParameterGroupStatus
