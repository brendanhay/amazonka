{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftServerLess.UpdateWorkgroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a workgroup with the specified configuration settings.
module Amazonka.RedshiftServerLess.UpdateWorkgroup
  ( -- * Creating a Request
    UpdateWorkgroup (..),
    newUpdateWorkgroup,

    -- * Request Lenses
    updateWorkgroup_securityGroupIds,
    updateWorkgroup_baseCapacity,
    updateWorkgroup_publiclyAccessible,
    updateWorkgroup_configParameters,
    updateWorkgroup_enhancedVpcRouting,
    updateWorkgroup_subnetIds,
    updateWorkgroup_workgroupName,

    -- * Destructuring the Response
    UpdateWorkgroupResponse (..),
    newUpdateWorkgroupResponse,

    -- * Response Lenses
    updateWorkgroupResponse_httpStatus,
    updateWorkgroupResponse_workgroup,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkgroup' smart constructor.
data UpdateWorkgroup = UpdateWorkgroup'
  { -- | An array of security group IDs to associate with the workgroup.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The new base data warehouse capacity in Redshift Processing Units
    -- (RPUs).
    baseCapacity :: Prelude.Maybe Prelude.Int,
    -- | A value that specifies whether the workgroup can be accessible from a
    -- public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | An array of parameters to set for advanced control over a database. The
    -- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
    -- @search_path@, and @max_query_execution_time@.
    configParameters :: Prelude.Maybe [ConfigParameter],
    -- | The value that specifies whether to turn on enhanced virtual private
    -- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
    -- traffic through your VPC.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | An array of VPC subnet IDs to associate with the workgroup.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the workgroup to update.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkgroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'updateWorkgroup_securityGroupIds' - An array of security group IDs to associate with the workgroup.
--
-- 'baseCapacity', 'updateWorkgroup_baseCapacity' - The new base data warehouse capacity in Redshift Processing Units
-- (RPUs).
--
-- 'publiclyAccessible', 'updateWorkgroup_publiclyAccessible' - A value that specifies whether the workgroup can be accessible from a
-- public network.
--
-- 'configParameters', 'updateWorkgroup_configParameters' - An array of parameters to set for advanced control over a database. The
-- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
-- @search_path@, and @max_query_execution_time@.
--
-- 'enhancedVpcRouting', 'updateWorkgroup_enhancedVpcRouting' - The value that specifies whether to turn on enhanced virtual private
-- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
-- traffic through your VPC.
--
-- 'subnetIds', 'updateWorkgroup_subnetIds' - An array of VPC subnet IDs to associate with the workgroup.
--
-- 'workgroupName', 'updateWorkgroup_workgroupName' - The name of the workgroup to update.
newUpdateWorkgroup ::
  -- | 'workgroupName'
  Prelude.Text ->
  UpdateWorkgroup
newUpdateWorkgroup pWorkgroupName_ =
  UpdateWorkgroup'
    { securityGroupIds =
        Prelude.Nothing,
      baseCapacity = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      configParameters = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      workgroupName = pWorkgroupName_
    }

-- | An array of security group IDs to associate with the workgroup.
updateWorkgroup_securityGroupIds :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe [Prelude.Text])
updateWorkgroup_securityGroupIds = Lens.lens (\UpdateWorkgroup' {securityGroupIds} -> securityGroupIds) (\s@UpdateWorkgroup' {} a -> s {securityGroupIds = a} :: UpdateWorkgroup) Prelude.. Lens.mapping Lens.coerced

-- | The new base data warehouse capacity in Redshift Processing Units
-- (RPUs).
updateWorkgroup_baseCapacity :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe Prelude.Int)
updateWorkgroup_baseCapacity = Lens.lens (\UpdateWorkgroup' {baseCapacity} -> baseCapacity) (\s@UpdateWorkgroup' {} a -> s {baseCapacity = a} :: UpdateWorkgroup)

-- | A value that specifies whether the workgroup can be accessible from a
-- public network.
updateWorkgroup_publiclyAccessible :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe Prelude.Bool)
updateWorkgroup_publiclyAccessible = Lens.lens (\UpdateWorkgroup' {publiclyAccessible} -> publiclyAccessible) (\s@UpdateWorkgroup' {} a -> s {publiclyAccessible = a} :: UpdateWorkgroup)

-- | An array of parameters to set for advanced control over a database. The
-- options are @datestyle@, @enable_user_activity_logging@, @query_group@,
-- @search_path@, and @max_query_execution_time@.
updateWorkgroup_configParameters :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe [ConfigParameter])
updateWorkgroup_configParameters = Lens.lens (\UpdateWorkgroup' {configParameters} -> configParameters) (\s@UpdateWorkgroup' {} a -> s {configParameters = a} :: UpdateWorkgroup) Prelude.. Lens.mapping Lens.coerced

-- | The value that specifies whether to turn on enhanced virtual private
-- cloud (VPC) routing, which forces Amazon Redshift Serverless to route
-- traffic through your VPC.
updateWorkgroup_enhancedVpcRouting :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe Prelude.Bool)
updateWorkgroup_enhancedVpcRouting = Lens.lens (\UpdateWorkgroup' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@UpdateWorkgroup' {} a -> s {enhancedVpcRouting = a} :: UpdateWorkgroup)

-- | An array of VPC subnet IDs to associate with the workgroup.
updateWorkgroup_subnetIds :: Lens.Lens' UpdateWorkgroup (Prelude.Maybe [Prelude.Text])
updateWorkgroup_subnetIds = Lens.lens (\UpdateWorkgroup' {subnetIds} -> subnetIds) (\s@UpdateWorkgroup' {} a -> s {subnetIds = a} :: UpdateWorkgroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the workgroup to update.
updateWorkgroup_workgroupName :: Lens.Lens' UpdateWorkgroup Prelude.Text
updateWorkgroup_workgroupName = Lens.lens (\UpdateWorkgroup' {workgroupName} -> workgroupName) (\s@UpdateWorkgroup' {} a -> s {workgroupName = a} :: UpdateWorkgroup)

instance Core.AWSRequest UpdateWorkgroup where
  type
    AWSResponse UpdateWorkgroup =
      UpdateWorkgroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkgroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workgroup")
      )

instance Prelude.Hashable UpdateWorkgroup where
  hashWithSalt _salt UpdateWorkgroup' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` baseCapacity
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` configParameters
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData UpdateWorkgroup where
  rnf UpdateWorkgroup' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf baseCapacity
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf configParameters
      `Prelude.seq` Prelude.rnf enhancedVpcRouting
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders UpdateWorkgroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.UpdateWorkgroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkgroup where
  toJSON UpdateWorkgroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("baseCapacity" Data..=) Prelude.<$> baseCapacity,
            ("publiclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("configParameters" Data..=)
              Prelude.<$> configParameters,
            ("enhancedVpcRouting" Data..=)
              Prelude.<$> enhancedVpcRouting,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath UpdateWorkgroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkgroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkgroupResponse' smart constructor.
data UpdateWorkgroupResponse = UpdateWorkgroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated workgroup object.
    workgroup :: Workgroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkgroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkgroupResponse_httpStatus' - The response's http status code.
--
-- 'workgroup', 'updateWorkgroupResponse_workgroup' - The updated workgroup object.
newUpdateWorkgroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workgroup'
  Workgroup ->
  UpdateWorkgroupResponse
newUpdateWorkgroupResponse pHttpStatus_ pWorkgroup_ =
  UpdateWorkgroupResponse'
    { httpStatus = pHttpStatus_,
      workgroup = pWorkgroup_
    }

-- | The response's http status code.
updateWorkgroupResponse_httpStatus :: Lens.Lens' UpdateWorkgroupResponse Prelude.Int
updateWorkgroupResponse_httpStatus = Lens.lens (\UpdateWorkgroupResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkgroupResponse' {} a -> s {httpStatus = a} :: UpdateWorkgroupResponse)

-- | The updated workgroup object.
updateWorkgroupResponse_workgroup :: Lens.Lens' UpdateWorkgroupResponse Workgroup
updateWorkgroupResponse_workgroup = Lens.lens (\UpdateWorkgroupResponse' {workgroup} -> workgroup) (\s@UpdateWorkgroupResponse' {} a -> s {workgroup = a} :: UpdateWorkgroupResponse)

instance Prelude.NFData UpdateWorkgroupResponse where
  rnf UpdateWorkgroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workgroup
