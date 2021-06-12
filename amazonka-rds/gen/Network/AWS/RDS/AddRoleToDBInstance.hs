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
-- Module      : Network.AWS.RDS.AddRoleToDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Identity and Access Management (IAM) role with a DB
-- instance.
--
-- To add a role to a DB instance, the status of the DB instance must be
-- @available@.
module Network.AWS.RDS.AddRoleToDBInstance
  ( -- * Creating a Request
    AddRoleToDBInstance (..),
    newAddRoleToDBInstance,

    -- * Request Lenses
    addRoleToDBInstance_dbInstanceIdentifier,
    addRoleToDBInstance_roleArn,
    addRoleToDBInstance_featureName,

    -- * Destructuring the Response
    AddRoleToDBInstanceResponse (..),
    newAddRoleToDBInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { -- | The name of the DB instance to associate the IAM role with.
    dbInstanceIdentifier :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB
    -- instance, for example @arn:aws:iam::123456789012:role\/AccessRole@.
    roleArn :: Core.Text,
    -- | The name of the feature for the DB instance that the IAM role is to be
    -- associated with. For the list of supported feature names, see
    -- DBEngineVersion.
    featureName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddRoleToDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'addRoleToDBInstance_dbInstanceIdentifier' - The name of the DB instance to associate the IAM role with.
--
-- 'roleArn', 'addRoleToDBInstance_roleArn' - The Amazon Resource Name (ARN) of the IAM role to associate with the DB
-- instance, for example @arn:aws:iam::123456789012:role\/AccessRole@.
--
-- 'featureName', 'addRoleToDBInstance_featureName' - The name of the feature for the DB instance that the IAM role is to be
-- associated with. For the list of supported feature names, see
-- DBEngineVersion.
newAddRoleToDBInstance ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  -- | 'featureName'
  Core.Text ->
  AddRoleToDBInstance
newAddRoleToDBInstance
  pDBInstanceIdentifier_
  pRoleArn_
  pFeatureName_ =
    AddRoleToDBInstance'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        roleArn = pRoleArn_,
        featureName = pFeatureName_
      }

-- | The name of the DB instance to associate the IAM role with.
addRoleToDBInstance_dbInstanceIdentifier :: Lens.Lens' AddRoleToDBInstance Core.Text
addRoleToDBInstance_dbInstanceIdentifier = Lens.lens (\AddRoleToDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AddRoleToDBInstance' {} a -> s {dbInstanceIdentifier = a} :: AddRoleToDBInstance)

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB
-- instance, for example @arn:aws:iam::123456789012:role\/AccessRole@.
addRoleToDBInstance_roleArn :: Lens.Lens' AddRoleToDBInstance Core.Text
addRoleToDBInstance_roleArn = Lens.lens (\AddRoleToDBInstance' {roleArn} -> roleArn) (\s@AddRoleToDBInstance' {} a -> s {roleArn = a} :: AddRoleToDBInstance)

-- | The name of the feature for the DB instance that the IAM role is to be
-- associated with. For the list of supported feature names, see
-- DBEngineVersion.
addRoleToDBInstance_featureName :: Lens.Lens' AddRoleToDBInstance Core.Text
addRoleToDBInstance_featureName = Lens.lens (\AddRoleToDBInstance' {featureName} -> featureName) (\s@AddRoleToDBInstance' {} a -> s {featureName = a} :: AddRoleToDBInstance)

instance Core.AWSRequest AddRoleToDBInstance where
  type
    AWSResponse AddRoleToDBInstance =
      AddRoleToDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddRoleToDBInstanceResponse'

instance Core.Hashable AddRoleToDBInstance

instance Core.NFData AddRoleToDBInstance

instance Core.ToHeaders AddRoleToDBInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddRoleToDBInstance where
  toPath = Core.const "/"

instance Core.ToQuery AddRoleToDBInstance where
  toQuery AddRoleToDBInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddRoleToDBInstance" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "RoleArn" Core.=: roleArn,
        "FeatureName" Core.=: featureName
      ]

-- | /See:/ 'newAddRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddRoleToDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddRoleToDBInstanceResponse ::
  AddRoleToDBInstanceResponse
newAddRoleToDBInstanceResponse =
  AddRoleToDBInstanceResponse'

instance Core.NFData AddRoleToDBInstanceResponse
