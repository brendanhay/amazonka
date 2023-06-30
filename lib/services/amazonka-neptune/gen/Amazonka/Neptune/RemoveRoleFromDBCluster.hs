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
-- Module      : Amazonka.Neptune.RemoveRoleFromDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Identity and Access Management (IAM) role from a DB
-- cluster.
module Amazonka.Neptune.RemoveRoleFromDBCluster
  ( -- * Creating a Request
    RemoveRoleFromDBCluster (..),
    newRemoveRoleFromDBCluster,

    -- * Request Lenses
    removeRoleFromDBCluster_featureName,
    removeRoleFromDBCluster_dbClusterIdentifier,
    removeRoleFromDBCluster_roleArn,

    -- * Destructuring the Response
    RemoveRoleFromDBClusterResponse (..),
    newRemoveRoleFromDBClusterResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveRoleFromDBCluster' smart constructor.
data RemoveRoleFromDBCluster = RemoveRoleFromDBCluster'
  { -- | The name of the feature for the DB cluster that the IAM role is to be
    -- disassociated from. For the list of supported feature names, see
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster to disassociate the IAM role from.
    dbClusterIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the
    -- DB cluster, for example
    -- @arn:aws:iam::123456789012:role\/NeptuneAccessRole@.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureName', 'removeRoleFromDBCluster_featureName' - The name of the feature for the DB cluster that the IAM role is to be
-- disassociated from. For the list of supported feature names, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
--
-- 'dbClusterIdentifier', 'removeRoleFromDBCluster_dbClusterIdentifier' - The name of the DB cluster to disassociate the IAM role from.
--
-- 'roleArn', 'removeRoleFromDBCluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the
-- DB cluster, for example
-- @arn:aws:iam::123456789012:role\/NeptuneAccessRole@.
newRemoveRoleFromDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  RemoveRoleFromDBCluster
newRemoveRoleFromDBCluster
  pDBClusterIdentifier_
  pRoleArn_ =
    RemoveRoleFromDBCluster'
      { featureName =
          Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        roleArn = pRoleArn_
      }

-- | The name of the feature for the DB cluster that the IAM role is to be
-- disassociated from. For the list of supported feature names, see
-- <https://docs.aws.amazon.com/neptune/latest/userguide/api-other-apis.html#DescribeDBEngineVersions DescribeDBEngineVersions>.
removeRoleFromDBCluster_featureName :: Lens.Lens' RemoveRoleFromDBCluster (Prelude.Maybe Prelude.Text)
removeRoleFromDBCluster_featureName = Lens.lens (\RemoveRoleFromDBCluster' {featureName} -> featureName) (\s@RemoveRoleFromDBCluster' {} a -> s {featureName = a} :: RemoveRoleFromDBCluster)

-- | The name of the DB cluster to disassociate the IAM role from.
removeRoleFromDBCluster_dbClusterIdentifier :: Lens.Lens' RemoveRoleFromDBCluster Prelude.Text
removeRoleFromDBCluster_dbClusterIdentifier = Lens.lens (\RemoveRoleFromDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RemoveRoleFromDBCluster' {} a -> s {dbClusterIdentifier = a} :: RemoveRoleFromDBCluster)

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the
-- DB cluster, for example
-- @arn:aws:iam::123456789012:role\/NeptuneAccessRole@.
removeRoleFromDBCluster_roleArn :: Lens.Lens' RemoveRoleFromDBCluster Prelude.Text
removeRoleFromDBCluster_roleArn = Lens.lens (\RemoveRoleFromDBCluster' {roleArn} -> roleArn) (\s@RemoveRoleFromDBCluster' {} a -> s {roleArn = a} :: RemoveRoleFromDBCluster)

instance Core.AWSRequest RemoveRoleFromDBCluster where
  type
    AWSResponse RemoveRoleFromDBCluster =
      RemoveRoleFromDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      RemoveRoleFromDBClusterResponse'

instance Prelude.Hashable RemoveRoleFromDBCluster where
  hashWithSalt _salt RemoveRoleFromDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData RemoveRoleFromDBCluster where
  rnf RemoveRoleFromDBCluster' {..} =
    Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders RemoveRoleFromDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveRoleFromDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveRoleFromDBCluster where
  toQuery RemoveRoleFromDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemoveRoleFromDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "FeatureName" Data.=: featureName,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "RoleArn" Data.=: roleArn
      ]

-- | /See:/ 'newRemoveRoleFromDBClusterResponse' smart constructor.
data RemoveRoleFromDBClusterResponse = RemoveRoleFromDBClusterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveRoleFromDBClusterResponse ::
  RemoveRoleFromDBClusterResponse
newRemoveRoleFromDBClusterResponse =
  RemoveRoleFromDBClusterResponse'

instance
  Prelude.NFData
    RemoveRoleFromDBClusterResponse
  where
  rnf _ = ()
