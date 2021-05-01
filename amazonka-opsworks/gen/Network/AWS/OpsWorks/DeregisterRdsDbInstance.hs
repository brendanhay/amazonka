{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.DeregisterRdsDbInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon RDS instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeregisterRdsDbInstance
  ( -- * Creating a Request
    DeregisterRdsDbInstance (..),
    newDeregisterRdsDbInstance,

    -- * Request Lenses
    deregisterRdsDbInstance_rdsDbInstanceArn,

    -- * Destructuring the Response
    DeregisterRdsDbInstanceResponse (..),
    newDeregisterRdsDbInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterRdsDbInstance' smart constructor.
data DeregisterRdsDbInstance = DeregisterRdsDbInstance'
  { -- | The Amazon RDS instance\'s ARN.
    rdsDbInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterRdsDbInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsDbInstanceArn', 'deregisterRdsDbInstance_rdsDbInstanceArn' - The Amazon RDS instance\'s ARN.
newDeregisterRdsDbInstance ::
  -- | 'rdsDbInstanceArn'
  Prelude.Text ->
  DeregisterRdsDbInstance
newDeregisterRdsDbInstance pRdsDbInstanceArn_ =
  DeregisterRdsDbInstance'
    { rdsDbInstanceArn =
        pRdsDbInstanceArn_
    }

-- | The Amazon RDS instance\'s ARN.
deregisterRdsDbInstance_rdsDbInstanceArn :: Lens.Lens' DeregisterRdsDbInstance Prelude.Text
deregisterRdsDbInstance_rdsDbInstanceArn = Lens.lens (\DeregisterRdsDbInstance' {rdsDbInstanceArn} -> rdsDbInstanceArn) (\s@DeregisterRdsDbInstance' {} a -> s {rdsDbInstanceArn = a} :: DeregisterRdsDbInstance)

instance Prelude.AWSRequest DeregisterRdsDbInstance where
  type
    Rs DeregisterRdsDbInstance =
      DeregisterRdsDbInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeregisterRdsDbInstanceResponse'

instance Prelude.Hashable DeregisterRdsDbInstance

instance Prelude.NFData DeregisterRdsDbInstance

instance Prelude.ToHeaders DeregisterRdsDbInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DeregisterRdsDbInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterRdsDbInstance where
  toJSON DeregisterRdsDbInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RdsDbInstanceArn" Prelude..= rdsDbInstanceArn)
          ]
      )

instance Prelude.ToPath DeregisterRdsDbInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterRdsDbInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterRdsDbInstanceResponse' smart constructor.
data DeregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterRdsDbInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterRdsDbInstanceResponse ::
  DeregisterRdsDbInstanceResponse
newDeregisterRdsDbInstanceResponse =
  DeregisterRdsDbInstanceResponse'

instance
  Prelude.NFData
    DeregisterRdsDbInstanceResponse
