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
-- Module      : Network.AWS.RDS.RemoveRoleFromDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from a DB
-- instance.
module Network.AWS.RDS.RemoveRoleFromDBInstance
  ( -- * Creating a Request
    RemoveRoleFromDBInstance (..),
    newRemoveRoleFromDBInstance,

    -- * Request Lenses
    removeRoleFromDBInstance_dbInstanceIdentifier,
    removeRoleFromDBInstance_roleArn,
    removeRoleFromDBInstance_featureName,

    -- * Destructuring the Response
    RemoveRoleFromDBInstanceResponse (..),
    newRemoveRoleFromDBInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveRoleFromDBInstance' smart constructor.
data RemoveRoleFromDBInstance = RemoveRoleFromDBInstance'
  { -- | The name of the DB instance to disassociate the IAM role from.
    dbInstanceIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the
    -- DB instance, for example, @arn:aws:iam::123456789012:role\/AccessRole@.
    roleArn :: Prelude.Text,
    -- | The name of the feature for the DB instance that the IAM role is to be
    -- disassociated from. For the list of supported feature names, see
    -- @DBEngineVersion@.
    featureName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'removeRoleFromDBInstance_dbInstanceIdentifier' - The name of the DB instance to disassociate the IAM role from.
--
-- 'roleArn', 'removeRoleFromDBInstance_roleArn' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the
-- DB instance, for example, @arn:aws:iam::123456789012:role\/AccessRole@.
--
-- 'featureName', 'removeRoleFromDBInstance_featureName' - The name of the feature for the DB instance that the IAM role is to be
-- disassociated from. For the list of supported feature names, see
-- @DBEngineVersion@.
newRemoveRoleFromDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
  RemoveRoleFromDBInstance
newRemoveRoleFromDBInstance
  pDBInstanceIdentifier_
  pRoleArn_
  pFeatureName_ =
    RemoveRoleFromDBInstance'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        roleArn = pRoleArn_,
        featureName = pFeatureName_
      }

-- | The name of the DB instance to disassociate the IAM role from.
removeRoleFromDBInstance_dbInstanceIdentifier :: Lens.Lens' RemoveRoleFromDBInstance Prelude.Text
removeRoleFromDBInstance_dbInstanceIdentifier = Lens.lens (\RemoveRoleFromDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@RemoveRoleFromDBInstance' {} a -> s {dbInstanceIdentifier = a} :: RemoveRoleFromDBInstance)

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the
-- DB instance, for example, @arn:aws:iam::123456789012:role\/AccessRole@.
removeRoleFromDBInstance_roleArn :: Lens.Lens' RemoveRoleFromDBInstance Prelude.Text
removeRoleFromDBInstance_roleArn = Lens.lens (\RemoveRoleFromDBInstance' {roleArn} -> roleArn) (\s@RemoveRoleFromDBInstance' {} a -> s {roleArn = a} :: RemoveRoleFromDBInstance)

-- | The name of the feature for the DB instance that the IAM role is to be
-- disassociated from. For the list of supported feature names, see
-- @DBEngineVersion@.
removeRoleFromDBInstance_featureName :: Lens.Lens' RemoveRoleFromDBInstance Prelude.Text
removeRoleFromDBInstance_featureName = Lens.lens (\RemoveRoleFromDBInstance' {featureName} -> featureName) (\s@RemoveRoleFromDBInstance' {} a -> s {featureName = a} :: RemoveRoleFromDBInstance)

instance Core.AWSRequest RemoveRoleFromDBInstance where
  type
    AWSResponse RemoveRoleFromDBInstance =
      RemoveRoleFromDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      RemoveRoleFromDBInstanceResponse'

instance Prelude.Hashable RemoveRoleFromDBInstance

instance Prelude.NFData RemoveRoleFromDBInstance

instance Core.ToHeaders RemoveRoleFromDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RemoveRoleFromDBInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveRoleFromDBInstance where
  toQuery RemoveRoleFromDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RemoveRoleFromDBInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "RoleArn" Core.=: roleArn,
        "FeatureName" Core.=: featureName
      ]

-- | /See:/ 'newRemoveRoleFromDBInstanceResponse' smart constructor.
data RemoveRoleFromDBInstanceResponse = RemoveRoleFromDBInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRoleFromDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveRoleFromDBInstanceResponse ::
  RemoveRoleFromDBInstanceResponse
newRemoveRoleFromDBInstanceResponse =
  RemoveRoleFromDBInstanceResponse'

instance
  Prelude.NFData
    RemoveRoleFromDBInstanceResponse
