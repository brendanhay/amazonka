{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Elastic IP address with a specified stack. An address can be registered with only one stack at a time. If the address is already registered, you must first deregister it by calling 'DeregisterElasticIp' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterElasticIP
  ( -- * Creating a request
    RegisterElasticIP (..),
    mkRegisterElasticIP,

    -- ** Request lenses
    reiElasticIP,
    reiStackId,

    -- * Destructuring the response
    RegisterElasticIPResponse (..),
    mkRegisterElasticIPResponse,

    -- ** Response lenses
    reirsElasticIP,
    reirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterElasticIP' smart constructor.
data RegisterElasticIP = RegisterElasticIP'
  { elasticIP :: Lude.Text,
    stackId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterElasticIP' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The Elastic IP address.
-- * 'stackId' - The stack ID.
mkRegisterElasticIP ::
  -- | 'elasticIP'
  Lude.Text ->
  -- | 'stackId'
  Lude.Text ->
  RegisterElasticIP
mkRegisterElasticIP pElasticIP_ pStackId_ =
  RegisterElasticIP' {elasticIP = pElasticIP_, stackId = pStackId_}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticIP :: Lens.Lens' RegisterElasticIP Lude.Text
reiElasticIP = Lens.lens (elasticIP :: RegisterElasticIP -> Lude.Text) (\s a -> s {elasticIP = a} :: RegisterElasticIP)
{-# DEPRECATED reiElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiStackId :: Lens.Lens' RegisterElasticIP Lude.Text
reiStackId = Lens.lens (stackId :: RegisterElasticIP -> Lude.Text) (\s a -> s {stackId = a} :: RegisterElasticIP)
{-# DEPRECATED reiStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest RegisterElasticIP where
  type Rs RegisterElasticIP = RegisterElasticIPResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterElasticIPResponse'
            Lude.<$> (x Lude..?> "ElasticIp") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterElasticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.RegisterElasticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterElasticIP where
  toJSON RegisterElasticIP' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ElasticIp" Lude..= elasticIP),
            Lude.Just ("StackId" Lude..= stackId)
          ]
      )

instance Lude.ToPath RegisterElasticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterElasticIP where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @RegisterElasticIp@ request.
--
-- /See:/ 'mkRegisterElasticIPResponse' smart constructor.
data RegisterElasticIPResponse = RegisterElasticIPResponse'
  { elasticIP ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterElasticIPResponse' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The Elastic IP address.
-- * 'responseStatus' - The response status code.
mkRegisterElasticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterElasticIPResponse
mkRegisterElasticIPResponse pResponseStatus_ =
  RegisterElasticIPResponse'
    { elasticIP = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirsElasticIP :: Lens.Lens' RegisterElasticIPResponse (Lude.Maybe Lude.Text)
reirsElasticIP = Lens.lens (elasticIP :: RegisterElasticIPResponse -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: RegisterElasticIPResponse)
{-# DEPRECATED reirsElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirsResponseStatus :: Lens.Lens' RegisterElasticIPResponse Lude.Int
reirsResponseStatus = Lens.lens (responseStatus :: RegisterElasticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterElasticIPResponse)
{-# DEPRECATED reirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
