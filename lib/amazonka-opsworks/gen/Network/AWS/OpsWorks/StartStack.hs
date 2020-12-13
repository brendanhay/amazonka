{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StartStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a stack's instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StartStack
  ( -- * Creating a request
    StartStack (..),
    mkStartStack,

    -- ** Request lenses
    ssfStackId,

    -- * Destructuring the response
    StartStackResponse (..),
    mkStartStackResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartStack' smart constructor.
newtype StartStack = StartStack'
  { -- | The stack ID.
    stackId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartStack' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
mkStartStack ::
  -- | 'stackId'
  Lude.Text ->
  StartStack
mkStartStack pStackId_ = StartStack' {stackId = pStackId_}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssfStackId :: Lens.Lens' StartStack Lude.Text
ssfStackId = Lens.lens (stackId :: StartStack -> Lude.Text) (\s a -> s {stackId = a} :: StartStack)
{-# DEPRECATED ssfStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest StartStack where
  type Rs StartStack = StartStackResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull StartStackResponse'

instance Lude.ToHeaders StartStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.StartStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartStack where
  toJSON StartStack' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StackId" Lude..= stackId)])

instance Lude.ToPath StartStack where
  toPath = Lude.const "/"

instance Lude.ToQuery StartStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartStackResponse' smart constructor.
data StartStackResponse = StartStackResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartStackResponse' with the minimum fields required to make a request.
mkStartStackResponse ::
  StartStackResponse
mkStartStackResponse = StartStackResponse'
