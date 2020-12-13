{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a human task user interface (worker task template).
--
-- To see a list of human task user interfaces (work task templates) in your account, use . When you delete a worker task template, it no longer appears when you call @ListHumanTaskUis@ .
module Network.AWS.SageMaker.DeleteHumanTaskUi
  ( -- * Creating a request
    DeleteHumanTaskUi (..),
    mkDeleteHumanTaskUi,

    -- ** Request lenses
    dhtuHumanTaskUiName,

    -- * Destructuring the response
    DeleteHumanTaskUiResponse (..),
    mkDeleteHumanTaskUiResponse,

    -- ** Response lenses
    dhtursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteHumanTaskUi' smart constructor.
newtype DeleteHumanTaskUi = DeleteHumanTaskUi'
  { -- | The name of the human task user interface (work task template) you want to delete.
    humanTaskUiName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHumanTaskUi' with the minimum fields required to make a request.
--
-- * 'humanTaskUiName' - The name of the human task user interface (work task template) you want to delete.
mkDeleteHumanTaskUi ::
  -- | 'humanTaskUiName'
  Lude.Text ->
  DeleteHumanTaskUi
mkDeleteHumanTaskUi pHumanTaskUiName_ =
  DeleteHumanTaskUi' {humanTaskUiName = pHumanTaskUiName_}

-- | The name of the human task user interface (work task template) you want to delete.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtuHumanTaskUiName :: Lens.Lens' DeleteHumanTaskUi Lude.Text
dhtuHumanTaskUiName = Lens.lens (humanTaskUiName :: DeleteHumanTaskUi -> Lude.Text) (\s a -> s {humanTaskUiName = a} :: DeleteHumanTaskUi)
{-# DEPRECATED dhtuHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

instance Lude.AWSRequest DeleteHumanTaskUi where
  type Rs DeleteHumanTaskUi = DeleteHumanTaskUiResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteHumanTaskUiResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteHumanTaskUi where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteHumanTaskUi" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteHumanTaskUi where
  toJSON DeleteHumanTaskUi' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("HumanTaskUiName" Lude..= humanTaskUiName)]
      )

instance Lude.ToPath DeleteHumanTaskUi where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteHumanTaskUi where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteHumanTaskUiResponse' smart constructor.
newtype DeleteHumanTaskUiResponse = DeleteHumanTaskUiResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteHumanTaskUiResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteHumanTaskUiResponse
mkDeleteHumanTaskUiResponse pResponseStatus_ =
  DeleteHumanTaskUiResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhtursResponseStatus :: Lens.Lens' DeleteHumanTaskUiResponse Lude.Int
dhtursResponseStatus = Lens.lens (responseStatus :: DeleteHumanTaskUiResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteHumanTaskUiResponse)
{-# DEPRECATED dhtursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
