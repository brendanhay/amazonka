{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StopStack
  ( -- * Creating a request
    StopStack (..),
    mkStopStack,

    -- ** Request lenses
    stoStackId,

    -- * Destructuring the response
    StopStackResponse (..),
    mkStopStackResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopStack' smart constructor.
newtype StopStack = StopStack' {stackId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStack' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
mkStopStack ::
  -- | 'stackId'
  Lude.Text ->
  StopStack
mkStopStack pStackId_ = StopStack' {stackId = pStackId_}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stoStackId :: Lens.Lens' StopStack Lude.Text
stoStackId = Lens.lens (stackId :: StopStack -> Lude.Text) (\s a -> s {stackId = a} :: StopStack)
{-# DEPRECATED stoStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest StopStack where
  type Rs StopStack = StopStackResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull StopStackResponse'

instance Lude.ToHeaders StopStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.StopStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopStack where
  toJSON StopStack' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StackId" Lude..= stackId)])

instance Lude.ToPath StopStack where
  toPath = Lude.const "/"

instance Lude.ToQuery StopStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopStackResponse' smart constructor.
data StopStackResponse = StopStackResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopStackResponse' with the minimum fields required to make a request.
mkStopStackResponse ::
  StopStackResponse
mkStopStackResponse = StopStackResponse'
