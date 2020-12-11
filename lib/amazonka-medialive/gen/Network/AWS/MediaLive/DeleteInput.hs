{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the input end point
module Network.AWS.MediaLive.DeleteInput
  ( -- * Creating a request
    DeleteInput (..),
    mkDeleteInput,

    -- ** Request lenses
    diInputId,

    -- * Destructuring the response
    DeleteInputResponse (..),
    mkDeleteInputResponse,

    -- ** Response lenses
    dirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteInputRequest
--
-- /See:/ 'mkDeleteInput' smart constructor.
newtype DeleteInput = DeleteInput' {inputId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInput' with the minimum fields required to make a request.
--
-- * 'inputId' - Unique ID of the input
mkDeleteInput ::
  -- | 'inputId'
  Lude.Text ->
  DeleteInput
mkDeleteInput pInputId_ = DeleteInput' {inputId = pInputId_}

-- | Unique ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInputId :: Lens.Lens' DeleteInput Lude.Text
diInputId = Lens.lens (inputId :: DeleteInput -> Lude.Text) (\s a -> s {inputId = a} :: DeleteInput)
{-# DEPRECATED diInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance Lude.AWSRequest DeleteInput where
  type Rs DeleteInput = DeleteInputResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteInputResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteInput where
  toPath DeleteInput' {..} =
    Lude.mconcat ["/prod/inputs/", Lude.toBS inputId]

instance Lude.ToQuery DeleteInput where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteInputResponse
--
-- /See:/ 'mkDeleteInputResponse' smart constructor.
newtype DeleteInputResponse = DeleteInputResponse'
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

-- | Creates a value of 'DeleteInputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteInputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInputResponse
mkDeleteInputResponse pResponseStatus_ =
  DeleteInputResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteInputResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInputResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
