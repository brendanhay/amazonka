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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterStatus

-- | A cluster parameter group that is associated with an Amazon Redshift
-- cluster.
--
-- /See:/ 'newAwsRedshiftClusterClusterParameterGroup' smart constructor.
data AwsRedshiftClusterClusterParameterGroup = AwsRedshiftClusterClusterParameterGroup'
  { -- | The list of parameter statuses.
    clusterParameterStatusList :: Prelude.Maybe [AwsRedshiftClusterClusterParameterStatus],
    -- | The status of updates to the parameters.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterParameterStatusList', 'awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList' - The list of parameter statuses.
--
-- 'parameterApplyStatus', 'awsRedshiftClusterClusterParameterGroup_parameterApplyStatus' - The status of updates to the parameters.
--
-- 'parameterGroupName', 'awsRedshiftClusterClusterParameterGroup_parameterGroupName' - The name of the parameter group.
newAwsRedshiftClusterClusterParameterGroup ::
  AwsRedshiftClusterClusterParameterGroup
newAwsRedshiftClusterClusterParameterGroup =
  AwsRedshiftClusterClusterParameterGroup'
    { clusterParameterStatusList =
        Prelude.Nothing,
      parameterApplyStatus =
        Prelude.Nothing,
      parameterGroupName =
        Prelude.Nothing
    }

-- | The list of parameter statuses.
awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList :: Lens.Lens' AwsRedshiftClusterClusterParameterGroup (Prelude.Maybe [AwsRedshiftClusterClusterParameterStatus])
awsRedshiftClusterClusterParameterGroup_clusterParameterStatusList = Lens.lens (\AwsRedshiftClusterClusterParameterGroup' {clusterParameterStatusList} -> clusterParameterStatusList) (\s@AwsRedshiftClusterClusterParameterGroup' {} a -> s {clusterParameterStatusList = a} :: AwsRedshiftClusterClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The status of updates to the parameters.
awsRedshiftClusterClusterParameterGroup_parameterApplyStatus :: Lens.Lens' AwsRedshiftClusterClusterParameterGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterParameterGroup_parameterApplyStatus = Lens.lens (\AwsRedshiftClusterClusterParameterGroup' {parameterApplyStatus} -> parameterApplyStatus) (\s@AwsRedshiftClusterClusterParameterGroup' {} a -> s {parameterApplyStatus = a} :: AwsRedshiftClusterClusterParameterGroup)

-- | The name of the parameter group.
awsRedshiftClusterClusterParameterGroup_parameterGroupName :: Lens.Lens' AwsRedshiftClusterClusterParameterGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterParameterGroup_parameterGroupName = Lens.lens (\AwsRedshiftClusterClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@AwsRedshiftClusterClusterParameterGroup' {} a -> s {parameterGroupName = a} :: AwsRedshiftClusterClusterParameterGroup)

instance
  Data.FromJSON
    AwsRedshiftClusterClusterParameterGroup
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterClusterParameterGroup"
      ( \x ->
          AwsRedshiftClusterClusterParameterGroup'
            Prelude.<$> ( x
                            Data..:? "ClusterParameterStatusList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ParameterApplyStatus")
            Prelude.<*> (x Data..:? "ParameterGroupName")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterParameterGroup
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterClusterParameterGroup' {..} =
      _salt
        `Prelude.hashWithSalt` clusterParameterStatusList
        `Prelude.hashWithSalt` parameterApplyStatus
        `Prelude.hashWithSalt` parameterGroupName

instance
  Prelude.NFData
    AwsRedshiftClusterClusterParameterGroup
  where
  rnf AwsRedshiftClusterClusterParameterGroup' {..} =
    Prelude.rnf clusterParameterStatusList `Prelude.seq`
      Prelude.rnf parameterApplyStatus `Prelude.seq`
        Prelude.rnf parameterGroupName

instance
  Data.ToJSON
    AwsRedshiftClusterClusterParameterGroup
  where
  toJSON AwsRedshiftClusterClusterParameterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterParameterStatusList" Data..=)
              Prelude.<$> clusterParameterStatusList,
            ("ParameterApplyStatus" Data..=)
              Prelude.<$> parameterApplyStatus,
            ("ParameterGroupName" Data..=)
              Prelude.<$> parameterGroupName
          ]
      )
