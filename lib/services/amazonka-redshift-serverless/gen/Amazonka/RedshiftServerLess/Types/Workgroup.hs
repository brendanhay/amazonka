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
-- Module      : Amazonka.RedshiftServerLess.Types.Workgroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.Workgroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.ConfigParameter
import Amazonka.RedshiftServerLess.Types.Endpoint
import Amazonka.RedshiftServerLess.Types.WorkgroupStatus

-- | The collection of computing resources from which an endpoint is created.
--
-- /See:/ 'newWorkgroup' smart constructor.
data Workgroup = Workgroup'
  { -- | An array of security group IDs to associate with the workgroup.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The namespace the workgroup is associated with.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The base data warehouse capacity of the workgroup in Redshift Processing
    -- Units (RPUs).
    baseCapacity :: Prelude.Maybe Prelude.Int,
    -- | The creation date of the workgroup.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the workgroup.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the workgroup.
    status :: Prelude.Maybe WorkgroupStatus,
    -- | A value that specifies whether the workgroup can be accessible from a
    -- public network
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | An array of parameters to set for finer control over a database. The
    -- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
    -- @search_path@, and @max_query_execution_time@.
    configParameters :: Prelude.Maybe [ConfigParameter],
    -- | The value that specifies whether to enable enhanced virtual private
    -- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
    -- traffic through your VPC.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | The endpoint that is created from the workgroup.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The unique identifier of the workgroup.
    workgroupId :: Prelude.Maybe Prelude.Text,
    -- | An array of subnet IDs the workgroup is associated with.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) that links to the workgroup.
    workgroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workgroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'workgroup_securityGroupIds' - An array of security group IDs to associate with the workgroup.
--
-- 'namespaceName', 'workgroup_namespaceName' - The namespace the workgroup is associated with.
--
-- 'baseCapacity', 'workgroup_baseCapacity' - The base data warehouse capacity of the workgroup in Redshift Processing
-- Units (RPUs).
--
-- 'creationDate', 'workgroup_creationDate' - The creation date of the workgroup.
--
-- 'workgroupName', 'workgroup_workgroupName' - The name of the workgroup.
--
-- 'status', 'workgroup_status' - The status of the workgroup.
--
-- 'publiclyAccessible', 'workgroup_publiclyAccessible' - A value that specifies whether the workgroup can be accessible from a
-- public network
--
-- 'configParameters', 'workgroup_configParameters' - An array of parameters to set for finer control over a database. The
-- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
-- @search_path@, and @max_query_execution_time@.
--
-- 'enhancedVpcRouting', 'workgroup_enhancedVpcRouting' - The value that specifies whether to enable enhanced virtual private
-- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
-- traffic through your VPC.
--
-- 'endpoint', 'workgroup_endpoint' - The endpoint that is created from the workgroup.
--
-- 'workgroupId', 'workgroup_workgroupId' - The unique identifier of the workgroup.
--
-- 'subnetIds', 'workgroup_subnetIds' - An array of subnet IDs the workgroup is associated with.
--
-- 'workgroupArn', 'workgroup_workgroupArn' - The Amazon Resource Name (ARN) that links to the workgroup.
newWorkgroup ::
  Workgroup
newWorkgroup =
  Workgroup'
    { securityGroupIds = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      baseCapacity = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      status = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      configParameters = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      workgroupId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      workgroupArn = Prelude.Nothing
    }

-- | An array of security group IDs to associate with the workgroup.
workgroup_securityGroupIds :: Lens.Lens' Workgroup (Prelude.Maybe [Prelude.Text])
workgroup_securityGroupIds = Lens.lens (\Workgroup' {securityGroupIds} -> securityGroupIds) (\s@Workgroup' {} a -> s {securityGroupIds = a} :: Workgroup) Prelude.. Lens.mapping Lens.coerced

-- | The namespace the workgroup is associated with.
workgroup_namespaceName :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Text)
workgroup_namespaceName = Lens.lens (\Workgroup' {namespaceName} -> namespaceName) (\s@Workgroup' {} a -> s {namespaceName = a} :: Workgroup)

-- | The base data warehouse capacity of the workgroup in Redshift Processing
-- Units (RPUs).
workgroup_baseCapacity :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Int)
workgroup_baseCapacity = Lens.lens (\Workgroup' {baseCapacity} -> baseCapacity) (\s@Workgroup' {} a -> s {baseCapacity = a} :: Workgroup)

-- | The creation date of the workgroup.
workgroup_creationDate :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.UTCTime)
workgroup_creationDate = Lens.lens (\Workgroup' {creationDate} -> creationDate) (\s@Workgroup' {} a -> s {creationDate = a} :: Workgroup) Prelude.. Lens.mapping Core._Time

-- | The name of the workgroup.
workgroup_workgroupName :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Text)
workgroup_workgroupName = Lens.lens (\Workgroup' {workgroupName} -> workgroupName) (\s@Workgroup' {} a -> s {workgroupName = a} :: Workgroup)

-- | The status of the workgroup.
workgroup_status :: Lens.Lens' Workgroup (Prelude.Maybe WorkgroupStatus)
workgroup_status = Lens.lens (\Workgroup' {status} -> status) (\s@Workgroup' {} a -> s {status = a} :: Workgroup)

-- | A value that specifies whether the workgroup can be accessible from a
-- public network
workgroup_publiclyAccessible :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Bool)
workgroup_publiclyAccessible = Lens.lens (\Workgroup' {publiclyAccessible} -> publiclyAccessible) (\s@Workgroup' {} a -> s {publiclyAccessible = a} :: Workgroup)

-- | An array of parameters to set for finer control over a database. The
-- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
-- @search_path@, and @max_query_execution_time@.
workgroup_configParameters :: Lens.Lens' Workgroup (Prelude.Maybe [ConfigParameter])
workgroup_configParameters = Lens.lens (\Workgroup' {configParameters} -> configParameters) (\s@Workgroup' {} a -> s {configParameters = a} :: Workgroup) Prelude.. Lens.mapping Lens.coerced

-- | The value that specifies whether to enable enhanced virtual private
-- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
-- traffic through your VPC.
workgroup_enhancedVpcRouting :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Bool)
workgroup_enhancedVpcRouting = Lens.lens (\Workgroup' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@Workgroup' {} a -> s {enhancedVpcRouting = a} :: Workgroup)

-- | The endpoint that is created from the workgroup.
workgroup_endpoint :: Lens.Lens' Workgroup (Prelude.Maybe Endpoint)
workgroup_endpoint = Lens.lens (\Workgroup' {endpoint} -> endpoint) (\s@Workgroup' {} a -> s {endpoint = a} :: Workgroup)

-- | The unique identifier of the workgroup.
workgroup_workgroupId :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Text)
workgroup_workgroupId = Lens.lens (\Workgroup' {workgroupId} -> workgroupId) (\s@Workgroup' {} a -> s {workgroupId = a} :: Workgroup)

-- | An array of subnet IDs the workgroup is associated with.
workgroup_subnetIds :: Lens.Lens' Workgroup (Prelude.Maybe [Prelude.Text])
workgroup_subnetIds = Lens.lens (\Workgroup' {subnetIds} -> subnetIds) (\s@Workgroup' {} a -> s {subnetIds = a} :: Workgroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) that links to the workgroup.
workgroup_workgroupArn :: Lens.Lens' Workgroup (Prelude.Maybe Prelude.Text)
workgroup_workgroupArn = Lens.lens (\Workgroup' {workgroupArn} -> workgroupArn) (\s@Workgroup' {} a -> s {workgroupArn = a} :: Workgroup)

instance Core.FromJSON Workgroup where
  parseJSON =
    Core.withObject
      "Workgroup"
      ( \x ->
          Workgroup'
            Prelude.<$> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "namespaceName")
            Prelude.<*> (x Core..:? "baseCapacity")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "workgroupName")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "publiclyAccessible")
            Prelude.<*> ( x Core..:? "configParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "enhancedVpcRouting")
            Prelude.<*> (x Core..:? "endpoint")
            Prelude.<*> (x Core..:? "workgroupId")
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "workgroupArn")
      )

instance Prelude.Hashable Workgroup where
  hashWithSalt _salt Workgroup' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` baseCapacity
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` configParameters
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` workgroupId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` workgroupArn

instance Prelude.NFData Workgroup where
  rnf Workgroup' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf baseCapacity
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf configParameters
      `Prelude.seq` Prelude.rnf enhancedVpcRouting
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf workgroupId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf workgroupArn
