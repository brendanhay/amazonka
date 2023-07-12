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
-- Module      : Amazonka.OpsWorks.DisassociateElasticIp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from its instance. The address
-- remains registered with the stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DisassociateElasticIp
  ( -- * Creating a Request
    DisassociateElasticIp (..),
    newDisassociateElasticIp,

    -- * Request Lenses
    disassociateElasticIp_elasticIp,

    -- * Destructuring the Response
    DisassociateElasticIpResponse (..),
    newDisassociateElasticIpResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateElasticIp' smart constructor.
data DisassociateElasticIp = DisassociateElasticIp'
  { -- | The Elastic IP address.
    elasticIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'disassociateElasticIp_elasticIp' - The Elastic IP address.
newDisassociateElasticIp ::
  -- | 'elasticIp'
  Prelude.Text ->
  DisassociateElasticIp
newDisassociateElasticIp pElasticIp_ =
  DisassociateElasticIp' {elasticIp = pElasticIp_}

-- | The Elastic IP address.
disassociateElasticIp_elasticIp :: Lens.Lens' DisassociateElasticIp Prelude.Text
disassociateElasticIp_elasticIp = Lens.lens (\DisassociateElasticIp' {elasticIp} -> elasticIp) (\s@DisassociateElasticIp' {} a -> s {elasticIp = a} :: DisassociateElasticIp)

instance Core.AWSRequest DisassociateElasticIp where
  type
    AWSResponse DisassociateElasticIp =
      DisassociateElasticIpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisassociateElasticIpResponse'

instance Prelude.Hashable DisassociateElasticIp where
  hashWithSalt _salt DisassociateElasticIp' {..} =
    _salt `Prelude.hashWithSalt` elasticIp

instance Prelude.NFData DisassociateElasticIp where
  rnf DisassociateElasticIp' {..} =
    Prelude.rnf elasticIp

instance Data.ToHeaders DisassociateElasticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DisassociateElasticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateElasticIp where
  toJSON DisassociateElasticIp' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ElasticIp" Data..= elasticIp)]
      )

instance Data.ToPath DisassociateElasticIp where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateElasticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateElasticIpResponse' smart constructor.
data DisassociateElasticIpResponse = DisassociateElasticIpResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateElasticIpResponse ::
  DisassociateElasticIpResponse
newDisassociateElasticIpResponse =
  DisassociateElasticIpResponse'

instance Prelude.NFData DisassociateElasticIpResponse where
  rnf _ = ()
