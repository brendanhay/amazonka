{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified stack. You must first delete all instances, layers, and apps or deregister registered instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html Shut Down a Stack> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteStack
  ( -- * Creating a request
    DeleteStack (..),
    mkDeleteStack,

    -- ** Request lenses
    dsStackId,

    -- * Destructuring the response
    DeleteStackResponse (..),
    mkDeleteStackResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStack' smart constructor.
newtype DeleteStack = DeleteStack' {stackId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
mkDeleteStack ::
  -- | 'stackId'
  Lude.Text ->
  DeleteStack
mkDeleteStack pStackId_ = DeleteStack' {stackId = pStackId_}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackId :: Lens.Lens' DeleteStack Lude.Text
dsStackId = Lens.lens (stackId :: DeleteStack -> Lude.Text) (\s a -> s {stackId = a} :: DeleteStack)
{-# DEPRECATED dsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DeleteStack where
  type Rs DeleteStack = DeleteStackResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeleteStackResponse'

instance Lude.ToHeaders DeleteStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeleteStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStack where
  toJSON DeleteStack' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StackId" Lude..= stackId)])

instance Lude.ToPath DeleteStack where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStackResponse' smart constructor.
data DeleteStackResponse = DeleteStackResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
mkDeleteStackResponse ::
  DeleteStackResponse
mkDeleteStackResponse = DeleteStackResponse'
