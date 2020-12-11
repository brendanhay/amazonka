{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Connect instance.
module Network.AWS.Connect.DeleteInstance
  ( -- * Creating a request
    DeleteInstance (..),
    mkDeleteInstance,

    -- ** Request lenses
    dInstanceId,

    -- * Destructuring the response
    DeleteInstanceResponse (..),
    mkDeleteInstanceResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstance' smart constructor.
newtype DeleteInstance = DeleteInstance' {instanceId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDeleteInstance ::
  -- | 'instanceId'
  Lude.Text ->
  DeleteInstance
mkDeleteInstance pInstanceId_ =
  DeleteInstance' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstanceId :: Lens.Lens' DeleteInstance Lude.Text
dInstanceId = Lens.lens (instanceId :: DeleteInstance -> Lude.Text) (\s a -> s {instanceId = a} :: DeleteInstance)
{-# DEPRECATED dInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = Req.delete connectService
  response = Res.receiveNull DeleteInstanceResponse'

instance Lude.ToHeaders DeleteInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteInstance where
  toPath DeleteInstance' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId]

instance Lude.ToQuery DeleteInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstanceResponse' with the minimum fields required to make a request.
mkDeleteInstanceResponse ::
  DeleteInstanceResponse
mkDeleteInstanceResponse = DeleteInstanceResponse'
