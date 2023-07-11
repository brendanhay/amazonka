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
-- Module      : Amazonka.OpsWorks.UpdateRdsDbInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.UpdateRdsDbInstance
  ( -- * Creating a Request
    UpdateRdsDbInstance (..),
    newUpdateRdsDbInstance,

    -- * Request Lenses
    updateRdsDbInstance_dbPassword,
    updateRdsDbInstance_dbUser,
    updateRdsDbInstance_rdsDbInstanceArn,

    -- * Destructuring the Response
    UpdateRdsDbInstanceResponse (..),
    newUpdateRdsDbInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRdsDbInstance' smart constructor.
data UpdateRdsDbInstance = UpdateRdsDbInstance'
  { -- | The database password.
    dbPassword :: Prelude.Maybe Prelude.Text,
    -- | The master user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The Amazon RDS instance\'s ARN.
    rdsDbInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbPassword', 'updateRdsDbInstance_dbPassword' - The database password.
--
-- 'dbUser', 'updateRdsDbInstance_dbUser' - The master user name.
--
-- 'rdsDbInstanceArn', 'updateRdsDbInstance_rdsDbInstanceArn' - The Amazon RDS instance\'s ARN.
newUpdateRdsDbInstance ::
  -- | 'rdsDbInstanceArn'
  Prelude.Text ->
  UpdateRdsDbInstance
newUpdateRdsDbInstance pRdsDbInstanceArn_ =
  UpdateRdsDbInstance'
    { dbPassword = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      rdsDbInstanceArn = pRdsDbInstanceArn_
    }

-- | The database password.
updateRdsDbInstance_dbPassword :: Lens.Lens' UpdateRdsDbInstance (Prelude.Maybe Prelude.Text)
updateRdsDbInstance_dbPassword = Lens.lens (\UpdateRdsDbInstance' {dbPassword} -> dbPassword) (\s@UpdateRdsDbInstance' {} a -> s {dbPassword = a} :: UpdateRdsDbInstance)

-- | The master user name.
updateRdsDbInstance_dbUser :: Lens.Lens' UpdateRdsDbInstance (Prelude.Maybe Prelude.Text)
updateRdsDbInstance_dbUser = Lens.lens (\UpdateRdsDbInstance' {dbUser} -> dbUser) (\s@UpdateRdsDbInstance' {} a -> s {dbUser = a} :: UpdateRdsDbInstance)

-- | The Amazon RDS instance\'s ARN.
updateRdsDbInstance_rdsDbInstanceArn :: Lens.Lens' UpdateRdsDbInstance Prelude.Text
updateRdsDbInstance_rdsDbInstanceArn = Lens.lens (\UpdateRdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@UpdateRdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: UpdateRdsDbInstance)

instance Core.AWSRequest UpdateRdsDbInstance where
  type
    AWSResponse UpdateRdsDbInstance =
      UpdateRdsDbInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateRdsDbInstanceResponse'

instance Prelude.Hashable UpdateRdsDbInstance where
  hashWithSalt _salt UpdateRdsDbInstance' {..} =
    _salt
      `Prelude.hashWithSalt` dbPassword
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` rdsDbInstanceArn

instance Prelude.NFData UpdateRdsDbInstance where
  rnf UpdateRdsDbInstance' {..} =
    Prelude.rnf dbPassword
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf rdsDbInstanceArn

instance Data.ToHeaders UpdateRdsDbInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UpdateRdsDbInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRdsDbInstance where
  toJSON UpdateRdsDbInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DbPassword" Data..=) Prelude.<$> dbPassword,
            ("DbUser" Data..=) Prelude.<$> dbUser,
            Prelude.Just
              ("RdsDbInstanceArn" Data..= rdsDbInstanceArn)
          ]
      )

instance Data.ToPath UpdateRdsDbInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRdsDbInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRdsDbInstanceResponse' smart constructor.
data UpdateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRdsDbInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRdsDbInstanceResponse ::
  UpdateRdsDbInstanceResponse
newUpdateRdsDbInstanceResponse =
  UpdateRdsDbInstanceResponse'

instance Prelude.NFData UpdateRdsDbInstanceResponse where
  rnf _ = ()
