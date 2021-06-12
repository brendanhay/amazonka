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
-- Module      : Network.AWS.OpsWorks.AssociateElasticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one of the stack\'s registered Elastic IP addresses with a
-- specified instance. The address must first be registered with the stack
-- by calling RegisterElasticIp. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.AssociateElasticIp
  ( -- * Creating a Request
    AssociateElasticIp (..),
    newAssociateElasticIp,

    -- * Request Lenses
    associateElasticIp_instanceId,
    associateElasticIp_elasticIp,

    -- * Destructuring the Response
    AssociateElasticIpResponse (..),
    newAssociateElasticIpResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateElasticIp' smart constructor.
data AssociateElasticIp = AssociateElasticIp'
  { -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The Elastic IP address.
    elasticIp :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateElasticIp_instanceId' - The instance ID.
--
-- 'elasticIp', 'associateElasticIp_elasticIp' - The Elastic IP address.
newAssociateElasticIp ::
  -- | 'elasticIp'
  Core.Text ->
  AssociateElasticIp
newAssociateElasticIp pElasticIp_ =
  AssociateElasticIp'
    { instanceId = Core.Nothing,
      elasticIp = pElasticIp_
    }

-- | The instance ID.
associateElasticIp_instanceId :: Lens.Lens' AssociateElasticIp (Core.Maybe Core.Text)
associateElasticIp_instanceId = Lens.lens (\AssociateElasticIp' {instanceId} -> instanceId) (\s@AssociateElasticIp' {} a -> s {instanceId = a} :: AssociateElasticIp)

-- | The Elastic IP address.
associateElasticIp_elasticIp :: Lens.Lens' AssociateElasticIp Core.Text
associateElasticIp_elasticIp = Lens.lens (\AssociateElasticIp' {elasticIp} -> elasticIp) (\s@AssociateElasticIp' {} a -> s {elasticIp = a} :: AssociateElasticIp)

instance Core.AWSRequest AssociateElasticIp where
  type
    AWSResponse AssociateElasticIp =
      AssociateElasticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AssociateElasticIpResponse'

instance Core.Hashable AssociateElasticIp

instance Core.NFData AssociateElasticIp

instance Core.ToHeaders AssociateElasticIp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.AssociateElasticIp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateElasticIp where
  toJSON AssociateElasticIp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            Core.Just ("ElasticIp" Core..= elasticIp)
          ]
      )

instance Core.ToPath AssociateElasticIp where
  toPath = Core.const "/"

instance Core.ToQuery AssociateElasticIp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateElasticIpResponse' smart constructor.
data AssociateElasticIpResponse = AssociateElasticIpResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateElasticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateElasticIpResponse ::
  AssociateElasticIpResponse
newAssociateElasticIpResponse =
  AssociateElasticIpResponse'

instance Core.NFData AssociateElasticIpResponse
