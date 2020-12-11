{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified layer. You must first stop and then delete all associated instances or unassign registered instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html How to Delete a Layer> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteLayer
  ( -- * Creating a request
    DeleteLayer (..),
    mkDeleteLayer,

    -- ** Request lenses
    dlLayerId,

    -- * Destructuring the response
    DeleteLayerResponse (..),
    mkDeleteLayerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLayer' smart constructor.
newtype DeleteLayer = DeleteLayer' {layerId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLayer' with the minimum fields required to make a request.
--
-- * 'layerId' - The layer ID.
mkDeleteLayer ::
  -- | 'layerId'
  Lude.Text ->
  DeleteLayer
mkDeleteLayer pLayerId_ = DeleteLayer' {layerId = pLayerId_}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLayerId :: Lens.Lens' DeleteLayer Lude.Text
dlLayerId = Lens.lens (layerId :: DeleteLayer -> Lude.Text) (\s a -> s {layerId = a} :: DeleteLayer)
{-# DEPRECATED dlLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest DeleteLayer where
  type Rs DeleteLayer = DeleteLayerResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeleteLayerResponse'

instance Lude.ToHeaders DeleteLayer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeleteLayer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLayer where
  toJSON DeleteLayer' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LayerId" Lude..= layerId)])

instance Lude.ToPath DeleteLayer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLayer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLayerResponse' smart constructor.
data DeleteLayerResponse = DeleteLayerResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLayerResponse' with the minimum fields required to make a request.
mkDeleteLayerResponse ::
  DeleteLayerResponse
mkDeleteLayerResponse = DeleteLayerResponse'
