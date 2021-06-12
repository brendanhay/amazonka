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
-- Module      : Network.AWS.OpsWorks.RegisterRdsDbInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.RegisterRdsDbInstance
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterRdsDbInstance' smart constructor.
data RegisterRdsDbInstance = RegisterRdsDbInstance'
  { -- | The stack ID.
    stackId :: Core.Text,
    -- | The Amazon RDS instance\'s ARN.
    rdsDbInstanceArn :: Core.Text,
    -- | The database\'s master user name.
    dbUser :: Core.Text,
    -- | The database password.
    dbPassword :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'rdsDbInstanceArn'
  Core.Text ->
  -- | 'dbUser'
  Core.Text ->
  -- | 'dbPassword'
  Core.Text ->
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
registerRdsDbInstance_stackId :: Lens.Lens' RegisterRdsDbInstance Core.Text
registerRdsDbInstance_stackId = Lens.lens (\RegisterRdsDbInstance' {stackId} -> stackId) (\s@RegisterRdsDbInstance' {} a -> s {stackId = a} :: RegisterRdsDbInstance)

-- | The Amazon RDS instance\'s ARN.
registerRdsDbInstance_rdsDbInstanceArn :: Lens.Lens' RegisterRdsDbInstance Core.Text
registerRdsDbInstance_rdsDbInstanceArn = Lens.lens (\RegisterRdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@RegisterRdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: RegisterRdsDbInstance)

-- | The database\'s master user name.
registerRdsDbInstance_dbUser :: Lens.Lens' RegisterRdsDbInstance Core.Text
registerRdsDbInstance_dbUser = Lens.lens (\RegisterRdsDbInstance' {dbUser} -> dbUser) (\s@RegisterRdsDbInstance' {} a -> s {dbUser = a} :: RegisterRdsDbInstance)

-- | The database password.
registerRdsDbInstance_dbPassword :: Lens.Lens' RegisterRdsDbInstance Core.Text
registerRdsDbInstance_dbPassword = Lens.lens (\RegisterRdsDbInstance' {dbPassword} -> dbPassword) (\s@RegisterRdsDbInstance' {} a -> s {dbPassword = a} :: RegisterRdsDbInstance)

instance Core.AWSRequest RegisterRdsDbInstance where
  type
    AWSResponse RegisterRdsDbInstance =
      RegisterRdsDbInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RegisterRdsDbInstanceResponse'

instance Core.Hashable RegisterRdsDbInstance

instance Core.NFData RegisterRdsDbInstance

instance Core.ToHeaders RegisterRdsDbInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RegisterRdsDbInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterRdsDbInstance where
  toJSON RegisterRdsDbInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            Core.Just
              ("RdsDbInstanceArn" Core..= rdsDbInstanceArn),
            Core.Just ("DbUser" Core..= dbUser),
            Core.Just ("DbPassword" Core..= dbPassword)
          ]
      )

instance Core.ToPath RegisterRdsDbInstance where
  toPath = Core.const "/"

instance Core.ToQuery RegisterRdsDbInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterRdsDbInstanceResponse' smart constructor.
data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterRdsDbInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterRdsDbInstanceResponse ::
  RegisterRdsDbInstanceResponse
newRegisterRdsDbInstanceResponse =
  RegisterRdsDbInstanceResponse'

instance Core.NFData RegisterRdsDbInstanceResponse
