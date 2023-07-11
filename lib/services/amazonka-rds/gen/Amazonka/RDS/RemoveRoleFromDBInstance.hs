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
-- Module      : Amazonka.RDS.RemoveRoleFromDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon Web Services Identity and Access Management
-- (IAM) role from a DB instance.
module Amazonka.RDS.RemoveRoleFromDBInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveRoleFromDBInstance' smart constructor.
data RemoveRoleFromDBInstance = RemoveRoleFromDBInstance'
  { -- | The name of the DB instance to disassociate the IAM role from.
    dbInstanceIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the
    -- DB instance, for example, @arn:aws:iam::123456789012:role\/AccessRole@.
    roleArn :: Prelude.Text,
    -- | The name of the feature for the DB instance that the IAM role is to be
    -- disassociated from. For information about supported feature names, see
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
-- disassociated from. For information about supported feature names, see
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
-- disassociated from. For information about supported feature names, see
-- @DBEngineVersion@.
removeRoleFromDBInstance_featureName :: Lens.Lens' RemoveRoleFromDBInstance Prelude.Text
removeRoleFromDBInstance_featureName = Lens.lens (\RemoveRoleFromDBInstance' {featureName} -> featureName) (\s@RemoveRoleFromDBInstance' {} a -> s {featureName = a} :: RemoveRoleFromDBInstance)

instance Core.AWSRequest RemoveRoleFromDBInstance where
  type
    AWSResponse RemoveRoleFromDBInstance =
      RemoveRoleFromDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      RemoveRoleFromDBInstanceResponse'

instance Prelude.Hashable RemoveRoleFromDBInstance where
  hashWithSalt _salt RemoveRoleFromDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` featureName

instance Prelude.NFData RemoveRoleFromDBInstance where
  rnf RemoveRoleFromDBInstance' {..} =
    Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf featureName

instance Data.ToHeaders RemoveRoleFromDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveRoleFromDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveRoleFromDBInstance where
  toQuery RemoveRoleFromDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemoveRoleFromDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "RoleArn" Data.=: roleArn,
        "FeatureName" Data.=: featureName
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
  where
  rnf _ = ()
