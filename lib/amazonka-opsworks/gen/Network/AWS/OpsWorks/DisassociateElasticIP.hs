{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DisassociateElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from its instance. The address remains registered with the stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DisassociateElasticIP
  ( -- * Creating a request
    DisassociateElasticIP (..),
    mkDisassociateElasticIP,

    -- ** Request lenses
    deiElasticIP,

    -- * Destructuring the response
    DisassociateElasticIPResponse (..),
    mkDisassociateElasticIPResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateElasticIP' smart constructor.
newtype DisassociateElasticIP = DisassociateElasticIP'
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

-- | Creates a value of 'DisassociateElasticIP' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The Elastic IP address.
mkDisassociateElasticIP ::
  -- | 'elasticIP'
  Lude.Text ->
  DisassociateElasticIP
mkDisassociateElasticIP pElasticIP_ =
  DisassociateElasticIP' {elasticIP = pElasticIP_}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiElasticIP :: Lens.Lens' DisassociateElasticIP Lude.Text
deiElasticIP = Lens.lens (elasticIP :: DisassociateElasticIP -> Lude.Text) (\s a -> s {elasticIP = a} :: DisassociateElasticIP)
{-# DEPRECATED deiElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

instance Lude.AWSRequest DisassociateElasticIP where
  type Rs DisassociateElasticIP = DisassociateElasticIPResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DisassociateElasticIPResponse'

instance Lude.ToHeaders DisassociateElasticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DisassociateElasticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateElasticIP where
  toJSON DisassociateElasticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ElasticIp" Lude..= elasticIP)])

instance Lude.ToPath DisassociateElasticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateElasticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateElasticIPResponse' smart constructor.
data DisassociateElasticIPResponse = DisassociateElasticIPResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateElasticIPResponse' with the minimum fields required to make a request.
mkDisassociateElasticIPResponse ::
  DisassociateElasticIPResponse
mkDisassociateElasticIPResponse = DisassociateElasticIPResponse'
