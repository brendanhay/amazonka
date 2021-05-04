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
-- Module      : Network.AWS.OpsWorks.DisassociateElasticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.DisassociateElasticIp
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateElasticIp' smart constructor.
data DisassociateElasticIp = DisassociateElasticIp'
  { -- | The Elastic IP address.
    elasticIp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DisassociateElasticIp where
  type
    Rs DisassociateElasticIp =
      DisassociateElasticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DisassociateElasticIpResponse'

instance Prelude.Hashable DisassociateElasticIp

instance Prelude.NFData DisassociateElasticIp

instance Prelude.ToHeaders DisassociateElasticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DisassociateElasticIp" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateElasticIp where
  toJSON DisassociateElasticIp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ElasticIp" Prelude..= elasticIp)]
      )

instance Prelude.ToPath DisassociateElasticIp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateElasticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateElasticIpResponse' smart constructor.
data DisassociateElasticIpResponse = DisassociateElasticIpResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateElasticIpResponse ::
  DisassociateElasticIpResponse
newDisassociateElasticIpResponse =
  DisassociateElasticIpResponse'

instance Prelude.NFData DisassociateElasticIpResponse
