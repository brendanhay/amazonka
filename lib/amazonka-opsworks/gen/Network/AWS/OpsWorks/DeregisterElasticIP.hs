{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Elastic IP address. The address can then be registered by another stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterElasticIP
  ( -- * Creating a request
    DeregisterElasticIP (..),
    mkDeregisterElasticIP,

    -- ** Request lenses
    deipElasticIP,

    -- * Destructuring the response
    DeregisterElasticIPResponse (..),
    mkDeregisterElasticIPResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterElasticIP' smart constructor.
newtype DeregisterElasticIP = DeregisterElasticIP'
  { elasticIP ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterElasticIP' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The Elastic IP address.
mkDeregisterElasticIP ::
  -- | 'elasticIP'
  Lude.Text ->
  DeregisterElasticIP
mkDeregisterElasticIP pElasticIP_ =
  DeregisterElasticIP' {elasticIP = pElasticIP_}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deipElasticIP :: Lens.Lens' DeregisterElasticIP Lude.Text
deipElasticIP = Lens.lens (elasticIP :: DeregisterElasticIP -> Lude.Text) (\s a -> s {elasticIP = a} :: DeregisterElasticIP)
{-# DEPRECATED deipElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

instance Lude.AWSRequest DeregisterElasticIP where
  type Rs DeregisterElasticIP = DeregisterElasticIPResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeregisterElasticIPResponse'

instance Lude.ToHeaders DeregisterElasticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeregisterElasticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterElasticIP where
  toJSON DeregisterElasticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ElasticIp" Lude..= elasticIP)])

instance Lude.ToPath DeregisterElasticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterElasticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterElasticIPResponse' smart constructor.
data DeregisterElasticIPResponse = DeregisterElasticIPResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterElasticIPResponse' with the minimum fields required to make a request.
mkDeregisterElasticIPResponse ::
  DeregisterElasticIPResponse
mkDeregisterElasticIPResponse = DeregisterElasticIPResponse'
