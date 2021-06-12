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
-- Module      : Network.AWS.OpsWorks.UpdateRdsDbInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon RDS instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateRdsDbInstance
  ( -- * Creating a Request
    UpdateRdsDbInstance (..),
    newUpdateRdsDbInstance,

    -- * Request Lenses
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- * Destructuring the Response
    UpdateRdsDbInstanceResponse (..),
    newUpdateRdsDbInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRdsDbInstance' smart constructor.
data UpdateRdsDbInstance = UpdateRdsDbInstance'
  { -- | The master user name.
    dbUser :: Core.Maybe Core.Text,
    -- | The database password.
    dbPassword :: Core.Maybe Core.Text,
    -- | The Amazon RDS instance\'s ARN.
    rdsDbInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'updateRdsDbInstance_dbUser' - The master user name.
--
-- 'dbPassword', 'updateRdsDbInstance_dbPassword' - The database password.
--
-- 'rdsDbInstanceArn', 'updateRdsDbInstance_rdsDbInstanceArn' - The Amazon RDS instance\'s ARN.
newUpdateRdsDbInstance ::
  -- | 'rdsDbInstanceArn'
  Core.Text ->
  UpdateRdsDbInstance
newUpdateRdsDbInstance pRdsDbInstanceArn_ =
  UpdateRdsDbInstance'
    { dbUser = Core.Nothing,
      dbPassword = Core.Nothing,
      rdsDbInstanceArn = pRdsDbInstanceArn_
    }

-- | The master user name.
updateRdsDbInstance_dbUser :: Lens.Lens' UpdateRdsDbInstance (Core.Maybe Core.Text)
updateRdsDbInstance_dbUser = Lens.lens (\UpdateRdsDbInstance' {dbUser} -> dbUser) (\s@UpdateRdsDbInstance' {} a -> s {dbUser = a} :: UpdateRdsDbInstance)

-- | The database password.
updateRdsDbInstance_dbPassword :: Lens.Lens' UpdateRdsDbInstance (Core.Maybe Core.Text)
updateRdsDbInstance_dbPassword = Lens.lens (\UpdateRdsDbInstance' {dbPassword} -> dbPassword) (\s@UpdateRdsDbInstance' {} a -> s {dbPassword = a} :: UpdateRdsDbInstance)

-- | The Amazon RDS instance\'s ARN.
updateRdsDbInstance_rdsDbInstanceArn :: Lens.Lens' UpdateRdsDbInstance Core.Text
updateRdsDbInstance_rdsDbInstanceArn = Lens.lens (\UpdateRdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@UpdateRdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: UpdateRdsDbInstance)

instance Core.AWSRequest UpdateRdsDbInstance where
  type
    AWSResponse UpdateRdsDbInstance =
      UpdateRdsDbInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateRdsDbInstanceResponse'

instance Core.Hashable UpdateRdsDbInstance

instance Core.NFData UpdateRdsDbInstance

instance Core.ToHeaders UpdateRdsDbInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateRdsDbInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRdsDbInstance where
  toJSON UpdateRdsDbInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DbUser" Core..=) Core.<$> dbUser,
            ("DbPassword" Core..=) Core.<$> dbPassword,
            Core.Just
              ("RdsDbInstanceArn" Core..= rdsDbInstanceArn)
          ]
      )

instance Core.ToPath UpdateRdsDbInstance where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRdsDbInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRdsDbInstanceResponse' smart constructor.
data UpdateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRdsDbInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRdsDbInstanceResponse ::
  UpdateRdsDbInstanceResponse
newUpdateRdsDbInstanceResponse =
  UpdateRdsDbInstanceResponse'

instance Core.NFData UpdateRdsDbInstanceResponse
