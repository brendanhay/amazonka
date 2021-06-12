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
-- Module      : Network.AWS.OpsWorks.RegisterElasticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Elastic IP address with a specified stack. An address can
-- be registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.RegisterElasticIp
  ( -- * Creating a Request
    RegisterElasticIp (..),
    newRegisterElasticIp,

    -- * Request Lenses
    registerElasticIp_elasticIp,
    registerElasticIp_stackId,

    -- * Destructuring the Response
    RegisterElasticIpResponse (..),
    newRegisterElasticIpResponse,

    -- * Response Lenses
    registerElasticIpResponse_elasticIp,
    registerElasticIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterElasticIp' smart constructor.
data RegisterElasticIp = RegisterElasticIp'
  { -- | The Elastic IP address.
    elasticIp :: Core.Text,
    -- | The stack ID.
    stackId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'registerElasticIp_elasticIp' - The Elastic IP address.
--
-- 'stackId', 'registerElasticIp_stackId' - The stack ID.
newRegisterElasticIp ::
  -- | 'elasticIp'
  Core.Text ->
  -- | 'stackId'
  Core.Text ->
  RegisterElasticIp
newRegisterElasticIp pElasticIp_ pStackId_ =
  RegisterElasticIp'
    { elasticIp = pElasticIp_,
      stackId = pStackId_
    }

-- | The Elastic IP address.
registerElasticIp_elasticIp :: Lens.Lens' RegisterElasticIp Core.Text
registerElasticIp_elasticIp = Lens.lens (\RegisterElasticIp' {elasticIp} -> elasticIp) (\s@RegisterElasticIp' {} a -> s {elasticIp = a} :: RegisterElasticIp)

-- | The stack ID.
registerElasticIp_stackId :: Lens.Lens' RegisterElasticIp Core.Text
registerElasticIp_stackId = Lens.lens (\RegisterElasticIp' {stackId} -> stackId) (\s@RegisterElasticIp' {} a -> s {stackId = a} :: RegisterElasticIp)

instance Core.AWSRequest RegisterElasticIp where
  type
    AWSResponse RegisterElasticIp =
      RegisterElasticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterElasticIpResponse'
            Core.<$> (x Core..?> "ElasticIp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterElasticIp

instance Core.NFData RegisterElasticIp

instance Core.ToHeaders RegisterElasticIp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.RegisterElasticIp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterElasticIp where
  toJSON RegisterElasticIp' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ElasticIp" Core..= elasticIp),
            Core.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath RegisterElasticIp where
  toPath = Core.const "/"

instance Core.ToQuery RegisterElasticIp where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @RegisterElasticIp@ request.
--
-- /See:/ 'newRegisterElasticIpResponse' smart constructor.
data RegisterElasticIpResponse = RegisterElasticIpResponse'
  { -- | The Elastic IP address.
    elasticIp :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'registerElasticIpResponse_elasticIp' - The Elastic IP address.
--
-- 'httpStatus', 'registerElasticIpResponse_httpStatus' - The response's http status code.
newRegisterElasticIpResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterElasticIpResponse
newRegisterElasticIpResponse pHttpStatus_ =
  RegisterElasticIpResponse'
    { elasticIp =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Elastic IP address.
registerElasticIpResponse_elasticIp :: Lens.Lens' RegisterElasticIpResponse (Core.Maybe Core.Text)
registerElasticIpResponse_elasticIp = Lens.lens (\RegisterElasticIpResponse' {elasticIp} -> elasticIp) (\s@RegisterElasticIpResponse' {} a -> s {elasticIp = a} :: RegisterElasticIpResponse)

-- | The response's http status code.
registerElasticIpResponse_httpStatus :: Lens.Lens' RegisterElasticIpResponse Core.Int
registerElasticIpResponse_httpStatus = Lens.lens (\RegisterElasticIpResponse' {httpStatus} -> httpStatus) (\s@RegisterElasticIpResponse' {} a -> s {httpStatus = a} :: RegisterElasticIpResponse)

instance Core.NFData RegisterElasticIpResponse
