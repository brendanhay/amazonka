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
-- Module      : Amazonka.OpsWorks.RegisterRdsDbInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon RDS instance with a stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.RegisterRdsDbInstance
  ( -- * Creating a Request
    RegisterRdsDbInstance (..),
    newRegisterRdsDbInstance,

    -- * Request Lenses
    registerRdsDbInstance_stackId,
    registerRdsDbInstance_rdsDbInstanceArn,
    registerRdsDbInstance_dbUser,
    registerRdsDbInstance_dbPassword,

    -- * Destructuring the Response
    RegisterRdsDbInstanceResponse (..),
    newRegisterRdsDbInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterRdsDbInstance' smart constructor.
data RegisterRdsDbInstance = RegisterRdsDbInstance'
  { -- | The stack ID.
    stackId :: Prelude.Text,
    -- | The Amazon RDS instance\'s ARN.
    rdsDbInstanceArn :: Prelude.Text,
    -- | The database\'s master user name.
    dbUser :: Prelude.Text,
    -- | The database password.
    dbPassword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterRdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'registerRdsDbInstance_stackId' - The stack ID.
--
-- 'rdsDbInstanceArn', 'registerRdsDbInstance_rdsDbInstanceArn' - The Amazon RDS instance\'s ARN.
--
-- 'dbUser', 'registerRdsDbInstance_dbUser' - The database\'s master user name.
--
-- 'dbPassword', 'registerRdsDbInstance_dbPassword' - The database password.
newRegisterRdsDbInstance ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'rdsDbInstanceArn'
  Prelude.Text ->
  -- | 'dbUser'
  Prelude.Text ->
  -- | 'dbPassword'
  Prelude.Text ->
  RegisterRdsDbInstance
newRegisterRdsDbInstance
  pStackId_
  pRdsDbInstanceArn_
  pDbUser_
  pDbPassword_ =
    RegisterRdsDbInstance'
      { stackId = pStackId_,
        rdsDbInstanceArn = pRdsDbInstanceArn_,
        dbUser = pDbUser_,
        dbPassword = pDbPassword_
      }

-- | The stack ID.
registerRdsDbInstance_stackId :: Lens.Lens' RegisterRdsDbInstance Prelude.Text
registerRdsDbInstance_stackId = Lens.lens (\RegisterRdsDbInstance' {stackId} -> stackId) (\s@RegisterRdsDbInstance' {} a -> s {stackId = a} :: RegisterRdsDbInstance)

-- | The Amazon RDS instance\'s ARN.
registerRdsDbInstance_rdsDbInstanceArn :: Lens.Lens' RegisterRdsDbInstance Prelude.Text
registerRdsDbInstance_rdsDbInstanceArn = Lens.lens (\RegisterRdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@RegisterRdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: RegisterRdsDbInstance)

-- | The database\'s master user name.
registerRdsDbInstance_dbUser :: Lens.Lens' RegisterRdsDbInstance Prelude.Text
registerRdsDbInstance_dbUser = Lens.lens (\RegisterRdsDbInstance' {dbUser} -> dbUser) (\s@RegisterRdsDbInstance' {} a -> s {dbUser = a} :: RegisterRdsDbInstance)

-- | The database password.
registerRdsDbInstance_dbPassword :: Lens.Lens' RegisterRdsDbInstance Prelude.Text
registerRdsDbInstance_dbPassword = Lens.lens (\RegisterRdsDbInstance' {dbPassword} -> dbPassword) (\s@RegisterRdsDbInstance' {} a -> s {dbPassword = a} :: RegisterRdsDbInstance)

instance Core.AWSRequest RegisterRdsDbInstance where
  type
    AWSResponse RegisterRdsDbInstance =
      RegisterRdsDbInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RegisterRdsDbInstanceResponse'

instance Prelude.Hashable RegisterRdsDbInstance where
  hashWithSalt _salt RegisterRdsDbInstance' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` rdsDbInstanceArn
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` dbPassword

instance Prelude.NFData RegisterRdsDbInstance where
  rnf RegisterRdsDbInstance' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf rdsDbInstanceArn
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf dbPassword

instance Data.ToHeaders RegisterRdsDbInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.RegisterRdsDbInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterRdsDbInstance where
  toJSON RegisterRdsDbInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StackId" Data..= stackId),
            Prelude.Just
              ("RdsDbInstanceArn" Data..= rdsDbInstanceArn),
            Prelude.Just ("DbUser" Data..= dbUser),
            Prelude.Just ("DbPassword" Data..= dbPassword)
          ]
      )

instance Data.ToPath RegisterRdsDbInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterRdsDbInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterRdsDbInstanceResponse' smart constructor.
data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterRdsDbInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterRdsDbInstanceResponse ::
  RegisterRdsDbInstanceResponse
newRegisterRdsDbInstanceResponse =
  RegisterRdsDbInstanceResponse'

instance Prelude.NFData RegisterRdsDbInstanceResponse where
  rnf _ = ()
