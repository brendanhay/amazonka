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
-- Module      : Network.AWS.Inspector.RegisterCrossAccountAccessRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the IAM role that grants Amazon Inspector access to AWS
-- Services needed to perform security assessments.
module Network.AWS.Inspector.RegisterCrossAccountAccessRole
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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterCrossAccountAccessRole' smart constructor.
data RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { -- | The ARN of the IAM role that grants Amazon Inspector access to AWS
    -- Services needed to perform security assessments.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    RegisterCrossAccountAccessRole
  where
  type
    Rs RegisterCrossAccountAccessRole =
      RegisterCrossAccountAccessRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterCrossAccountAccessRoleResponse'

instance
  Prelude.Hashable
    RegisterCrossAccountAccessRole

instance
  Prelude.NFData
    RegisterCrossAccountAccessRole

instance
  Prelude.ToHeaders
    RegisterCrossAccountAccessRole
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.RegisterCrossAccountAccessRole" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    RegisterCrossAccountAccessRole
  where
  toJSON RegisterCrossAccountAccessRole' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("roleArn" Prelude..= roleArn)]
      )

instance
  Prelude.ToPath
    RegisterCrossAccountAccessRole
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RegisterCrossAccountAccessRole
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
