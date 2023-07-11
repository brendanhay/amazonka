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
-- Module      : Amazonka.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
--
-- Only one IAM ARN (an IAM session ARN or IAM user ARN) is supported in
-- the request. You cannot use both.
module Amazonka.CodeDeploy.RegisterOnPremisesInstance
  ( -- * Creating a Request
    RegisterOnPremisesInstance (..),
    newRegisterOnPremisesInstance,

    -- * Request Lenses
    registerOnPremisesInstance_iamSessionArn,
    registerOnPremisesInstance_iamUserArn,
    registerOnPremisesInstance_instanceName,

    -- * Destructuring the Response
    RegisterOnPremisesInstanceResponse (..),
    newRegisterOnPremisesInstanceResponse,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of the register on-premises instance operation.
--
-- /See:/ 'newRegisterOnPremisesInstance' smart constructor.
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
  { -- | The ARN of the IAM session to associate with the on-premises instance.
    iamSessionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM user to associate with the on-premises instance.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the on-premises instance to register.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterOnPremisesInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamSessionArn', 'registerOnPremisesInstance_iamSessionArn' - The ARN of the IAM session to associate with the on-premises instance.
--
-- 'iamUserArn', 'registerOnPremisesInstance_iamUserArn' - The ARN of the IAM user to associate with the on-premises instance.
--
-- 'instanceName', 'registerOnPremisesInstance_instanceName' - The name of the on-premises instance to register.
newRegisterOnPremisesInstance ::
  -- | 'instanceName'
  Prelude.Text ->
  RegisterOnPremisesInstance
newRegisterOnPremisesInstance pInstanceName_ =
  RegisterOnPremisesInstance'
    { iamSessionArn =
        Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      instanceName = pInstanceName_
    }

-- | The ARN of the IAM session to associate with the on-premises instance.
registerOnPremisesInstance_iamSessionArn :: Lens.Lens' RegisterOnPremisesInstance (Prelude.Maybe Prelude.Text)
registerOnPremisesInstance_iamSessionArn = Lens.lens (\RegisterOnPremisesInstance' {iamSessionArn} -> iamSessionArn) (\s@RegisterOnPremisesInstance' {} a -> s {iamSessionArn = a} :: RegisterOnPremisesInstance)

-- | The ARN of the IAM user to associate with the on-premises instance.
registerOnPremisesInstance_iamUserArn :: Lens.Lens' RegisterOnPremisesInstance (Prelude.Maybe Prelude.Text)
registerOnPremisesInstance_iamUserArn = Lens.lens (\RegisterOnPremisesInstance' {iamUserArn} -> iamUserArn) (\s@RegisterOnPremisesInstance' {} a -> s {iamUserArn = a} :: RegisterOnPremisesInstance)

-- | The name of the on-premises instance to register.
registerOnPremisesInstance_instanceName :: Lens.Lens' RegisterOnPremisesInstance Prelude.Text
registerOnPremisesInstance_instanceName = Lens.lens (\RegisterOnPremisesInstance' {instanceName} -> instanceName) (\s@RegisterOnPremisesInstance' {} a -> s {instanceName = a} :: RegisterOnPremisesInstance)

instance Core.AWSRequest RegisterOnPremisesInstance where
  type
    AWSResponse RegisterOnPremisesInstance =
      RegisterOnPremisesInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RegisterOnPremisesInstanceResponse'

instance Prelude.Hashable RegisterOnPremisesInstance where
  hashWithSalt _salt RegisterOnPremisesInstance' {..} =
    _salt
      `Prelude.hashWithSalt` iamSessionArn
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData RegisterOnPremisesInstance where
  rnf RegisterOnPremisesInstance' {..} =
    Prelude.rnf iamSessionArn
      `Prelude.seq` Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders RegisterOnPremisesInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.RegisterOnPremisesInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterOnPremisesInstance where
  toJSON RegisterOnPremisesInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iamSessionArn" Data..=) Prelude.<$> iamSessionArn,
            ("iamUserArn" Data..=) Prelude.<$> iamUserArn,
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath RegisterOnPremisesInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterOnPremisesInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
