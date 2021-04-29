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
-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
--
-- Only one IAM ARN (an IAM session ARN or IAM user ARN) is supported in
-- the request. You cannot use both.
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
  ( -- * Creating a Request
    RegisterOnPremisesInstance (..),
    newRegisterOnPremisesInstance,

    -- * Request Lenses
    registerOnPremisesInstance_iamUserArn,
    registerOnPremisesInstance_iamSessionArn,
    registerOnPremisesInstance_instanceName,

    -- * Destructuring the Response
    RegisterOnPremisesInstanceResponse (..),
    newRegisterOnPremisesInstanceResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of the register on-premises instance operation.
--
-- /See:/ 'newRegisterOnPremisesInstance' smart constructor.
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
  { -- | The ARN of the IAM user to associate with the on-premises instance.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM session to associate with the on-premises instance.
    iamSessionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the on-premises instance to register.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterOnPremisesInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArn', 'registerOnPremisesInstance_iamUserArn' - The ARN of the IAM user to associate with the on-premises instance.
--
-- 'iamSessionArn', 'registerOnPremisesInstance_iamSessionArn' - The ARN of the IAM session to associate with the on-premises instance.
--
-- 'instanceName', 'registerOnPremisesInstance_instanceName' - The name of the on-premises instance to register.
newRegisterOnPremisesInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  RegisterOnPremisesInstance
newRegisterOnPremisesInstance pInstanceName_ =
  RegisterOnPremisesInstance'
    { iamUserArn =
        Prelude.Nothing,
      iamSessionArn = Prelude.Nothing,
      instanceName = pInstanceName_
    }

-- | The ARN of the IAM user to associate with the on-premises instance.
registerOnPremisesInstance_iamUserArn :: Lens.Lens' RegisterOnPremisesInstance (Prelude.Maybe Prelude.Text)
registerOnPremisesInstance_iamUserArn = Lens.lens (\RegisterOnPremisesInstance' {iamUserArn} -> iamUserArn) (\s@RegisterOnPremisesInstance' {} a -> s {iamUserArn = a} :: RegisterOnPremisesInstance)

-- | The ARN of the IAM session to associate with the on-premises instance.
registerOnPremisesInstance_iamSessionArn :: Lens.Lens' RegisterOnPremisesInstance (Prelude.Maybe Prelude.Text)
registerOnPremisesInstance_iamSessionArn = Lens.lens (\RegisterOnPremisesInstance' {iamSessionArn} -> iamSessionArn) (\s@RegisterOnPremisesInstance' {} a -> s {iamSessionArn = a} :: RegisterOnPremisesInstance)

-- | The name of the on-premises instance to register.
registerOnPremisesInstance_instanceName :: Lens.Lens' RegisterOnPremisesInstance Prelude.Text
registerOnPremisesInstance_instanceName = Lens.lens (\RegisterOnPremisesInstance' {instanceName} -> instanceName) (\s@RegisterOnPremisesInstance' {} a -> s {instanceName = a} :: RegisterOnPremisesInstance)

instance
  Prelude.AWSRequest
    RegisterOnPremisesInstance
  where
  type
    Rs RegisterOnPremisesInstance =
      RegisterOnPremisesInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterOnPremisesInstanceResponse'

instance Prelude.Hashable RegisterOnPremisesInstance

instance Prelude.NFData RegisterOnPremisesInstance

instance Prelude.ToHeaders RegisterOnPremisesInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.RegisterOnPremisesInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterOnPremisesInstance where
  toJSON RegisterOnPremisesInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("iamUserArn" Prelude..=) Prelude.<$> iamUserArn,
            ("iamSessionArn" Prelude..=)
              Prelude.<$> iamSessionArn,
            Prelude.Just
              ("instanceName" Prelude..= instanceName)
          ]
      )

instance Prelude.ToPath RegisterOnPremisesInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterOnPremisesInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterOnPremisesInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterOnPremisesInstanceResponse ::
  RegisterOnPremisesInstanceResponse
newRegisterOnPremisesInstanceResponse =
  RegisterOnPremisesInstanceResponse'

instance
  Prelude.NFData
    RegisterOnPremisesInstanceResponse
