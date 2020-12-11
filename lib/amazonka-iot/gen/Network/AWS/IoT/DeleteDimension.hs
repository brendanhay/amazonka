{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified dimension from your AWS account.
module Network.AWS.IoT.DeleteDimension
  ( -- * Creating a request
    DeleteDimension (..),
    mkDeleteDimension,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeleteDimensionResponse (..),
    mkDeleteDimensionResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDimension' smart constructor.
newtype DeleteDimension = DeleteDimension' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDimension' with the minimum fields required to make a request.
--
-- * 'name' - The unique identifier for the dimension that you want to delete.
mkDeleteDimension ::
  -- | 'name'
  Lude.Text ->
  DeleteDimension
mkDeleteDimension pName_ = DeleteDimension' {name = pName_}

-- | The unique identifier for the dimension that you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteDimension Lude.Text
dName = Lens.lens (name :: DeleteDimension -> Lude.Text) (\s a -> s {name = a} :: DeleteDimension)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteDimension where
  type Rs DeleteDimension = DeleteDimensionResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDimensionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDimension where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDimension where
  toPath DeleteDimension' {..} =
    Lude.mconcat ["/dimensions/", Lude.toBS name]

instance Lude.ToQuery DeleteDimension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDimensionResponse' smart constructor.
newtype DeleteDimensionResponse = DeleteDimensionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDimensionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDimensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDimensionResponse
mkDeleteDimensionResponse pResponseStatus_ =
  DeleteDimensionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDimensionResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDimensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDimensionResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
