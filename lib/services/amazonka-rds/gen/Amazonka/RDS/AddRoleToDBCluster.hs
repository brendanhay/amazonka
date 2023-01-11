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
-- Module      : Amazonka.RDS.AddRoleToDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Identity and Access Management (IAM) role with a DB
-- cluster.
module Amazonka.RDS.AddRoleToDBCluster
  ( -- * Creating a Request
    AddRoleToDBCluster (..),
    newAddRoleToDBCluster,

    -- * Request Lenses
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- * Destructuring the Response
    AddRoleToDBClusterResponse (..),
    newAddRoleToDBClusterResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddRoleToDBCluster' smart constructor.
data AddRoleToDBCluster = AddRoleToDBCluster'
  { -- | The name of the feature for the DB cluster that the IAM role is to be
    -- associated with. For information about supported feature names, see
    -- DBEngineVersion.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster to associate the IAM role with.
    dbClusterIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the
    -- Aurora DB cluster, for example
    -- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddRoleToDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureName', 'addRoleToDBCluster_featureName' - The name of the feature for the DB cluster that the IAM role is to be
-- associated with. For information about supported feature names, see
-- DBEngineVersion.
--
-- 'dbClusterIdentifier', 'addRoleToDBCluster_dbClusterIdentifier' - The name of the DB cluster to associate the IAM role with.
--
-- 'roleArn', 'addRoleToDBCluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role to associate with the
-- Aurora DB cluster, for example
-- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
newAddRoleToDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  AddRoleToDBCluster
newAddRoleToDBCluster pDBClusterIdentifier_ pRoleArn_ =
  AddRoleToDBCluster'
    { featureName = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      roleArn = pRoleArn_
    }

-- | The name of the feature for the DB cluster that the IAM role is to be
-- associated with. For information about supported feature names, see
-- DBEngineVersion.
addRoleToDBCluster_featureName :: Lens.Lens' AddRoleToDBCluster (Prelude.Maybe Prelude.Text)
addRoleToDBCluster_featureName = Lens.lens (\AddRoleToDBCluster' {featureName} -> featureName) (\s@AddRoleToDBCluster' {} a -> s {featureName = a} :: AddRoleToDBCluster)

-- | The name of the DB cluster to associate the IAM role with.
addRoleToDBCluster_dbClusterIdentifier :: Lens.Lens' AddRoleToDBCluster Prelude.Text
addRoleToDBCluster_dbClusterIdentifier = Lens.lens (\AddRoleToDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AddRoleToDBCluster' {} a -> s {dbClusterIdentifier = a} :: AddRoleToDBCluster)

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the
-- Aurora DB cluster, for example
-- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
addRoleToDBCluster_roleArn :: Lens.Lens' AddRoleToDBCluster Prelude.Text
addRoleToDBCluster_roleArn = Lens.lens (\AddRoleToDBCluster' {roleArn} -> roleArn) (\s@AddRoleToDBCluster' {} a -> s {roleArn = a} :: AddRoleToDBCluster)

instance Core.AWSRequest AddRoleToDBCluster where
  type
    AWSResponse AddRoleToDBCluster =
      AddRoleToDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AddRoleToDBClusterResponse'

instance Prelude.Hashable AddRoleToDBCluster where
  hashWithSalt _salt AddRoleToDBCluster' {..} =
    _salt `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AddRoleToDBCluster where
  rnf AddRoleToDBCluster' {..} =
    Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders AddRoleToDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddRoleToDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery AddRoleToDBCluster where
  toQuery AddRoleToDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AddRoleToDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "FeatureName" Data.=: featureName,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "RoleArn" Data.=: roleArn
      ]

-- | /See:/ 'newAddRoleToDBClusterResponse' smart constructor.
data AddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddRoleToDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddRoleToDBClusterResponse ::
  AddRoleToDBClusterResponse
newAddRoleToDBClusterResponse =
  AddRoleToDBClusterResponse'

instance Prelude.NFData AddRoleToDBClusterResponse where
  rnf _ = ()
