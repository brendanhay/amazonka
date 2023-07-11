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
-- Module      : Amazonka.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the
-- new list of subnets.
module Amazonka.Redshift.ModifyClusterSubnetGroup
  ( -- * Creating a Request
    ModifyClusterSubnetGroup (..),
    newModifyClusterSubnetGroup,

    -- * Request Lenses
    modifyClusterSubnetGroup_description,
    modifyClusterSubnetGroup_clusterSubnetGroupName,
    modifyClusterSubnetGroup_subnetIds,

    -- * Destructuring the Response
    ModifyClusterSubnetGroupResponse (..),
    newModifyClusterSubnetGroupResponse,

    -- * Response Lenses
    modifyClusterSubnetGroupResponse_clusterSubnetGroup,
    modifyClusterSubnetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
  { -- | A text description of the subnet group to be modified.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group to be modified.
    clusterSubnetGroupName :: Prelude.Text,
    -- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
    -- single request.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyClusterSubnetGroup_description' - A text description of the subnet group to be modified.
--
-- 'clusterSubnetGroupName', 'modifyClusterSubnetGroup_clusterSubnetGroupName' - The name of the subnet group to be modified.
--
-- 'subnetIds', 'modifyClusterSubnetGroup_subnetIds' - An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
newModifyClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Prelude.Text ->
  ModifyClusterSubnetGroup
newModifyClusterSubnetGroup pClusterSubnetGroupName_ =
  ModifyClusterSubnetGroup'
    { description =
        Prelude.Nothing,
      clusterSubnetGroupName = pClusterSubnetGroupName_,
      subnetIds = Prelude.mempty
    }

-- | A text description of the subnet group to be modified.
modifyClusterSubnetGroup_description :: Lens.Lens' ModifyClusterSubnetGroup (Prelude.Maybe Prelude.Text)
modifyClusterSubnetGroup_description = Lens.lens (\ModifyClusterSubnetGroup' {description} -> description) (\s@ModifyClusterSubnetGroup' {} a -> s {description = a} :: ModifyClusterSubnetGroup)

-- | The name of the subnet group to be modified.
modifyClusterSubnetGroup_clusterSubnetGroupName :: Lens.Lens' ModifyClusterSubnetGroup Prelude.Text
modifyClusterSubnetGroup_clusterSubnetGroupName = Lens.lens (\ModifyClusterSubnetGroup' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@ModifyClusterSubnetGroup' {} a -> s {clusterSubnetGroupName = a} :: ModifyClusterSubnetGroup)

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
modifyClusterSubnetGroup_subnetIds :: Lens.Lens' ModifyClusterSubnetGroup [Prelude.Text]
modifyClusterSubnetGroup_subnetIds = Lens.lens (\ModifyClusterSubnetGroup' {subnetIds} -> subnetIds) (\s@ModifyClusterSubnetGroup' {} a -> s {subnetIds = a} :: ModifyClusterSubnetGroup) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyClusterSubnetGroup where
  type
    AWSResponse ModifyClusterSubnetGroup =
      ModifyClusterSubnetGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyClusterSubnetGroupResult"
      ( \s h x ->
          ModifyClusterSubnetGroupResponse'
            Prelude.<$> (x Data..@? "ClusterSubnetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyClusterSubnetGroup where
  hashWithSalt _salt ModifyClusterSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData ModifyClusterSubnetGroup where
  rnf ModifyClusterSubnetGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders ModifyClusterSubnetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyClusterSubnetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyClusterSubnetGroup where
  toQuery ModifyClusterSubnetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyClusterSubnetGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Description" Data.=: description,
        "ClusterSubnetGroupName"
          Data.=: clusterSubnetGroupName,
        "SubnetIds"
          Data.=: Data.toQueryList "SubnetIdentifier" subnetIds
      ]

-- | /See:/ 'newModifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
  { clusterSubnetGroup :: Prelude.Maybe ClusterSubnetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSubnetGroup', 'modifyClusterSubnetGroupResponse_clusterSubnetGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyClusterSubnetGroupResponse_httpStatus' - The response's http status code.
newModifyClusterSubnetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyClusterSubnetGroupResponse
newModifyClusterSubnetGroupResponse pHttpStatus_ =
  ModifyClusterSubnetGroupResponse'
    { clusterSubnetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyClusterSubnetGroupResponse_clusterSubnetGroup :: Lens.Lens' ModifyClusterSubnetGroupResponse (Prelude.Maybe ClusterSubnetGroup)
modifyClusterSubnetGroupResponse_clusterSubnetGroup = Lens.lens (\ModifyClusterSubnetGroupResponse' {clusterSubnetGroup} -> clusterSubnetGroup) (\s@ModifyClusterSubnetGroupResponse' {} a -> s {clusterSubnetGroup = a} :: ModifyClusterSubnetGroupResponse)

-- | The response's http status code.
modifyClusterSubnetGroupResponse_httpStatus :: Lens.Lens' ModifyClusterSubnetGroupResponse Prelude.Int
modifyClusterSubnetGroupResponse_httpStatus = Lens.lens (\ModifyClusterSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyClusterSubnetGroupResponse' {} a -> s {httpStatus = a} :: ModifyClusterSubnetGroupResponse)

instance
  Prelude.NFData
    ModifyClusterSubnetGroupResponse
  where
  rnf ModifyClusterSubnetGroupResponse' {..} =
    Prelude.rnf clusterSubnetGroup
      `Prelude.seq` Prelude.rnf httpStatus
