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
-- Module      : Amazonka.Inspector.RegisterCrossAccountAccessRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the IAM role that grants Amazon Inspector access to AWS
-- Services needed to perform security assessments.
module Amazonka.Inspector.RegisterCrossAccountAccessRole
  ( -- * Creating a Request
    RegisterCrossAccountAccessRole (..),
    newRegisterCrossAccountAccessRole,

    -- * Request Lenses
    registerCrossAccountAccessRole_roleArn,

    -- * Destructuring the Response
    RegisterCrossAccountAccessRoleResponse (..),
    newRegisterCrossAccountAccessRoleResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterCrossAccountAccessRole' smart constructor.
data RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { -- | The ARN of the IAM role that grants Amazon Inspector access to AWS
    -- Services needed to perform security assessments.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCrossAccountAccessRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'registerCrossAccountAccessRole_roleArn' - The ARN of the IAM role that grants Amazon Inspector access to AWS
-- Services needed to perform security assessments.
newRegisterCrossAccountAccessRole ::
  -- | 'roleArn'
  Prelude.Text ->
  RegisterCrossAccountAccessRole
newRegisterCrossAccountAccessRole pRoleArn_ =
  RegisterCrossAccountAccessRole'
    { roleArn =
        pRoleArn_
    }

-- | The ARN of the IAM role that grants Amazon Inspector access to AWS
-- Services needed to perform security assessments.
registerCrossAccountAccessRole_roleArn :: Lens.Lens' RegisterCrossAccountAccessRole Prelude.Text
registerCrossAccountAccessRole_roleArn = Lens.lens (\RegisterCrossAccountAccessRole' {roleArn} -> roleArn) (\s@RegisterCrossAccountAccessRole' {} a -> s {roleArn = a} :: RegisterCrossAccountAccessRole)

instance
  Core.AWSRequest
    RegisterCrossAccountAccessRole
  where
  type
    AWSResponse RegisterCrossAccountAccessRole =
      RegisterCrossAccountAccessRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RegisterCrossAccountAccessRoleResponse'

instance
  Prelude.Hashable
    RegisterCrossAccountAccessRole
  where
  hashWithSalt
    _salt
    RegisterCrossAccountAccessRole' {..} =
      _salt `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    RegisterCrossAccountAccessRole
  where
  rnf RegisterCrossAccountAccessRole' {..} =
    Prelude.rnf roleArn

instance
  Data.ToHeaders
    RegisterCrossAccountAccessRole
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.RegisterCrossAccountAccessRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterCrossAccountAccessRole where
  toJSON RegisterCrossAccountAccessRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("roleArn" Data..= roleArn)]
      )

instance Data.ToPath RegisterCrossAccountAccessRole where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterCrossAccountAccessRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCrossAccountAccessRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterCrossAccountAccessRoleResponse ::
  RegisterCrossAccountAccessRoleResponse
newRegisterCrossAccountAccessRoleResponse =
  RegisterCrossAccountAccessRoleResponse'

instance
  Prelude.NFData
    RegisterCrossAccountAccessRoleResponse
  where
  rnf _ = ()
