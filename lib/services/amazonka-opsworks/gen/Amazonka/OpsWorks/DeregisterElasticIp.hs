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
-- Module      : Amazonka.OpsWorks.DeregisterElasticIp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DeregisterElasticIp
  ( -- * Creating a Request
    DeregisterElasticIp (..),
    newDeregisterElasticIp,

    -- * Request Lenses
    deregisterElasticIp_elasticIp,

    -- * Destructuring the Response
    DeregisterElasticIpResponse (..),
    newDeregisterElasticIpResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterElasticIp' smart constructor.
data DeregisterElasticIp = DeregisterElasticIp'
  { -- | The Elastic IP address.
    elasticIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'deregisterElasticIp_elasticIp' - The Elastic IP address.
newDeregisterElasticIp ::
  -- | 'elasticIp'
  Prelude.Text ->
  DeregisterElasticIp
newDeregisterElasticIp pElasticIp_ =
  DeregisterElasticIp' {elasticIp = pElasticIp_}

-- | The Elastic IP address.
deregisterElasticIp_elasticIp :: Lens.Lens' DeregisterElasticIp Prelude.Text
deregisterElasticIp_elasticIp = Lens.lens (\DeregisterElasticIp' {elasticIp} -> elasticIp) (\s@DeregisterElasticIp' {} a -> s {elasticIp = a} :: DeregisterElasticIp)

instance Core.AWSRequest DeregisterElasticIp where
  type
    AWSResponse DeregisterElasticIp =
      DeregisterElasticIpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeregisterElasticIpResponse'

instance Prelude.Hashable DeregisterElasticIp where
  hashWithSalt _salt DeregisterElasticIp' {..} =
    _salt `Prelude.hashWithSalt` elasticIp

instance Prelude.NFData DeregisterElasticIp where
  rnf DeregisterElasticIp' {..} = Prelude.rnf elasticIp

instance Core.ToHeaders DeregisterElasticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DeregisterElasticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeregisterElasticIp where
  toJSON DeregisterElasticIp' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ElasticIp" Core..= elasticIp)]
      )

instance Core.ToPath DeregisterElasticIp where
  toPath = Prelude.const "/"

instance Core.ToQuery DeregisterElasticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterElasticIpResponse' smart constructor.
data DeregisterElasticIpResponse = DeregisterElasticIpResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterElasticIpResponse ::
  DeregisterElasticIpResponse
newDeregisterElasticIpResponse =
  DeregisterElasticIpResponse'

instance Prelude.NFData DeregisterElasticIpResponse where
  rnf _ = ()
