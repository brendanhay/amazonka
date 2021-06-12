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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterCrossAccountAccessRole' smart constructor.
data RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { -- | The ARN of the IAM role that grants Amazon Inspector access to AWS
    -- Services needed to perform security assessments.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  RegisterCrossAccountAccessRole
newRegisterCrossAccountAccessRole pRoleArn_ =
  RegisterCrossAccountAccessRole'
    { roleArn =
        pRoleArn_
    }

-- | The ARN of the IAM role that grants Amazon Inspector access to AWS
-- Services needed to perform security assessments.
registerCrossAccountAccessRole_roleArn :: Lens.Lens' RegisterCrossAccountAccessRole Core.Text
registerCrossAccountAccessRole_roleArn = Lens.lens (\RegisterCrossAccountAccessRole' {roleArn} -> roleArn) (\s@RegisterCrossAccountAccessRole' {} a -> s {roleArn = a} :: RegisterCrossAccountAccessRole)

instance
  Core.AWSRequest
    RegisterCrossAccountAccessRole
  where
  type
    AWSResponse RegisterCrossAccountAccessRole =
      RegisterCrossAccountAccessRoleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterCrossAccountAccessRoleResponse'

instance Core.Hashable RegisterCrossAccountAccessRole

instance Core.NFData RegisterCrossAccountAccessRole

instance
  Core.ToHeaders
    RegisterCrossAccountAccessRole
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.RegisterCrossAccountAccessRole" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterCrossAccountAccessRole where
  toJSON RegisterCrossAccountAccessRole' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("roleArn" Core..= roleArn)]
      )

instance Core.ToPath RegisterCrossAccountAccessRole where
  toPath = Core.const "/"

instance Core.ToQuery RegisterCrossAccountAccessRole where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCrossAccountAccessRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterCrossAccountAccessRoleResponse ::
  RegisterCrossAccountAccessRoleResponse
newRegisterCrossAccountAccessRoleResponse =
  RegisterCrossAccountAccessRoleResponse'

instance
  Core.NFData
    RegisterCrossAccountAccessRoleResponse
