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
-- Module      : Amazonka.OpsWorks.RegisterElasticIp
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.RegisterElasticIp
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterElasticIp' smart constructor.
data RegisterElasticIp = RegisterElasticIp'
  { -- | The Elastic IP address.
    elasticIp :: Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'stackId'
  Prelude.Text ->
  RegisterElasticIp
newRegisterElasticIp pElasticIp_ pStackId_ =
  RegisterElasticIp'
    { elasticIp = pElasticIp_,
      stackId = pStackId_
    }

-- | The Elastic IP address.
registerElasticIp_elasticIp :: Lens.Lens' RegisterElasticIp Prelude.Text
registerElasticIp_elasticIp = Lens.lens (\RegisterElasticIp' {elasticIp} -> elasticIp) (\s@RegisterElasticIp' {} a -> s {elasticIp = a} :: RegisterElasticIp)

-- | The stack ID.
registerElasticIp_stackId :: Lens.Lens' RegisterElasticIp Prelude.Text
registerElasticIp_stackId = Lens.lens (\RegisterElasticIp' {stackId} -> stackId) (\s@RegisterElasticIp' {} a -> s {stackId = a} :: RegisterElasticIp)

instance Core.AWSRequest RegisterElasticIp where
  type
    AWSResponse RegisterElasticIp =
      RegisterElasticIpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterElasticIpResponse'
            Prelude.<$> (x Data..?> "ElasticIp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterElasticIp where
  hashWithSalt _salt RegisterElasticIp' {..} =
    _salt `Prelude.hashWithSalt` elasticIp
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData RegisterElasticIp where
  rnf RegisterElasticIp' {..} =
    Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders RegisterElasticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.RegisterElasticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterElasticIp where
  toJSON RegisterElasticIp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ElasticIp" Data..= elasticIp),
            Prelude.Just ("StackId" Data..= stackId)
          ]
      )

instance Data.ToPath RegisterElasticIp where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterElasticIp where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @RegisterElasticIp@ request.
--
-- /See:/ 'newRegisterElasticIpResponse' smart constructor.
data RegisterElasticIpResponse = RegisterElasticIpResponse'
  { -- | The Elastic IP address.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RegisterElasticIpResponse
newRegisterElasticIpResponse pHttpStatus_ =
  RegisterElasticIpResponse'
    { elasticIp =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Elastic IP address.
registerElasticIpResponse_elasticIp :: Lens.Lens' RegisterElasticIpResponse (Prelude.Maybe Prelude.Text)
registerElasticIpResponse_elasticIp = Lens.lens (\RegisterElasticIpResponse' {elasticIp} -> elasticIp) (\s@RegisterElasticIpResponse' {} a -> s {elasticIp = a} :: RegisterElasticIpResponse)

-- | The response's http status code.
registerElasticIpResponse_httpStatus :: Lens.Lens' RegisterElasticIpResponse Prelude.Int
registerElasticIpResponse_httpStatus = Lens.lens (\RegisterElasticIpResponse' {httpStatus} -> httpStatus) (\s@RegisterElasticIpResponse' {} a -> s {httpStatus = a} :: RegisterElasticIpResponse)

instance Prelude.NFData RegisterElasticIpResponse where
  rnf RegisterElasticIpResponse' {..} =
    Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf httpStatus
