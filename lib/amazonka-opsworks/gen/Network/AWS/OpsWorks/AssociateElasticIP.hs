{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssociateElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one of the stack's registered Elastic IP addresses with a specified instance. The address must first be registered with the stack by calling 'RegisterElasticIp' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssociateElasticIP
  ( -- * Creating a request
    AssociateElasticIP (..),
    mkAssociateElasticIP,

    -- ** Request lenses
    aeiInstanceId,
    aeiElasticIP,

    -- * Destructuring the response
    AssociateElasticIPResponse (..),
    mkAssociateElasticIPResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateElasticIP' smart constructor.
data AssociateElasticIP = AssociateElasticIP'
  { instanceId ::
      Lude.Maybe Lude.Text,
    elasticIP :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateElasticIP' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The Elastic IP address.
-- * 'instanceId' - The instance ID.
mkAssociateElasticIP ::
  -- | 'elasticIP'
  Lude.Text ->
  AssociateElasticIP
mkAssociateElasticIP pElasticIP_ =
  AssociateElasticIP'
    { instanceId = Lude.Nothing,
      elasticIP = pElasticIP_
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiInstanceId :: Lens.Lens' AssociateElasticIP (Lude.Maybe Lude.Text)
aeiInstanceId = Lens.lens (instanceId :: AssociateElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: AssociateElasticIP)
{-# DEPRECATED aeiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiElasticIP :: Lens.Lens' AssociateElasticIP Lude.Text
aeiElasticIP = Lens.lens (elasticIP :: AssociateElasticIP -> Lude.Text) (\s a -> s {elasticIP = a} :: AssociateElasticIP)
{-# DEPRECATED aeiElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

instance Lude.AWSRequest AssociateElasticIP where
  type Rs AssociateElasticIP = AssociateElasticIPResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull AssociateElasticIPResponse'

instance Lude.ToHeaders AssociateElasticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.AssociateElasticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateElasticIP where
  toJSON AssociateElasticIP' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            Lude.Just ("ElasticIp" Lude..= elasticIP)
          ]
      )

instance Lude.ToPath AssociateElasticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateElasticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateElasticIPResponse' smart constructor.
data AssociateElasticIPResponse = AssociateElasticIPResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateElasticIPResponse' with the minimum fields required to make a request.
mkAssociateElasticIPResponse ::
  AssociateElasticIPResponse
mkAssociateElasticIPResponse = AssociateElasticIPResponse'
