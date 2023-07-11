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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A security group that is associated with the cluster.
--
-- /See:/ 'newAwsRedshiftClusterClusterSecurityGroup' smart constructor.
data AwsRedshiftClusterClusterSecurityGroup = AwsRedshiftClusterClusterSecurityGroup'
  { -- | The name of the cluster security group.
    clusterSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster security group.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroupName', 'awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName' - The name of the cluster security group.
--
-- 'status', 'awsRedshiftClusterClusterSecurityGroup_status' - The status of the cluster security group.
newAwsRedshiftClusterClusterSecurityGroup ::
  AwsRedshiftClusterClusterSecurityGroup
newAwsRedshiftClusterClusterSecurityGroup =
  AwsRedshiftClusterClusterSecurityGroup'
    { clusterSecurityGroupName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the cluster security group.
awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName :: Lens.Lens' AwsRedshiftClusterClusterSecurityGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSecurityGroup_clusterSecurityGroupName = Lens.lens (\AwsRedshiftClusterClusterSecurityGroup' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@AwsRedshiftClusterClusterSecurityGroup' {} a -> s {clusterSecurityGroupName = a} :: AwsRedshiftClusterClusterSecurityGroup)

-- | The status of the cluster security group.
awsRedshiftClusterClusterSecurityGroup_status :: Lens.Lens' AwsRedshiftClusterClusterSecurityGroup (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterSecurityGroup_status = Lens.lens (\AwsRedshiftClusterClusterSecurityGroup' {status} -> status) (\s@AwsRedshiftClusterClusterSecurityGroup' {} a -> s {status = a} :: AwsRedshiftClusterClusterSecurityGroup)

instance
  Data.FromJSON
    AwsRedshiftClusterClusterSecurityGroup
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterClusterSecurityGroup"
      ( \x ->
          AwsRedshiftClusterClusterSecurityGroup'
            Prelude.<$> (x Data..:? "ClusterSecurityGroupName")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterSecurityGroup
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterClusterSecurityGroup' {..} =
      _salt
        `Prelude.hashWithSalt` clusterSecurityGroupName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRedshiftClusterClusterSecurityGroup
  where
  rnf AwsRedshiftClusterClusterSecurityGroup' {..} =
    Prelude.rnf clusterSecurityGroupName
      `Prelude.seq` Prelude.rnf status

instance
  Data.ToJSON
    AwsRedshiftClusterClusterSecurityGroup
  where
  toJSON AwsRedshiftClusterClusterSecurityGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterSecurityGroupName" Data..=)
              Prelude.<$> clusterSecurityGroupName,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
