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
-- Module      : Amazonka.RDS.AddRoleToDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon Web Services Identity and Access Management (IAM)
-- role with a DB instance.
--
-- To add a role to a DB instance, the status of the DB instance must be
-- @available@.
--
-- This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.AddRoleToDBInstance
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { -- | The name of the DB instance to associate the IAM role with.
    dbInstanceIdentifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB
    -- instance, for example @arn:aws:iam::123456789012:role\/AccessRole@.
    roleArn :: Prelude.Text,
    -- | The name of the feature for the DB instance that the IAM role is to be
    -- associated with. For information about supported feature names, see
    -- DBEngineVersion.
    featureName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- associated with. For information about supported feature names, see
-- DBEngineVersion.
newAddRoleToDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
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
addRoleToDBInstance_dbInstanceIdentifier :: Lens.Lens' AddRoleToDBInstance Prelude.Text
addRoleToDBInstance_dbInstanceIdentifier = Lens.lens (\AddRoleToDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AddRoleToDBInstance' {} a -> s {dbInstanceIdentifier = a} :: AddRoleToDBInstance)

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB
-- instance, for example @arn:aws:iam::123456789012:role\/AccessRole@.
addRoleToDBInstance_roleArn :: Lens.Lens' AddRoleToDBInstance Prelude.Text
addRoleToDBInstance_roleArn = Lens.lens (\AddRoleToDBInstance' {roleArn} -> roleArn) (\s@AddRoleToDBInstance' {} a -> s {roleArn = a} :: AddRoleToDBInstance)

-- | The name of the feature for the DB instance that the IAM role is to be
-- associated with. For information about supported feature names, see
-- DBEngineVersion.
addRoleToDBInstance_featureName :: Lens.Lens' AddRoleToDBInstance Prelude.Text
addRoleToDBInstance_featureName = Lens.lens (\AddRoleToDBInstance' {featureName} -> featureName) (\s@AddRoleToDBInstance' {} a -> s {featureName = a} :: AddRoleToDBInstance)

instance Core.AWSRequest AddRoleToDBInstance where
  type
    AWSResponse AddRoleToDBInstance =
      AddRoleToDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AddRoleToDBInstanceResponse'

instance Prelude.Hashable AddRoleToDBInstance where
  hashWithSalt _salt AddRoleToDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` featureName

instance Prelude.NFData AddRoleToDBInstance where
  rnf AddRoleToDBInstance' {..} =
    Prelude.rnf dbInstanceIdentifier `Prelude.seq`
      Prelude.rnf roleArn `Prelude.seq`
        Prelude.rnf featureName

instance Data.ToHeaders AddRoleToDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddRoleToDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery AddRoleToDBInstance where
  toQuery AddRoleToDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AddRoleToDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "RoleArn" Data.=: roleArn,
        "FeatureName" Data.=: featureName
      ]

-- | /See:/ 'newAddRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddRoleToDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddRoleToDBInstanceResponse ::
  AddRoleToDBInstanceResponse
newAddRoleToDBInstanceResponse =
  AddRoleToDBInstanceResponse'

instance Prelude.NFData AddRoleToDBInstanceResponse where
  rnf _ = ()
