{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
-- You cannot delete a TagOption if it is associated with a product or portfolio.
module Network.AWS.ServiceCatalog.DeleteTagOption
  ( -- * Creating a request
    DeleteTagOption (..),
    mkDeleteTagOption,

    -- ** Request lenses
    dtofId,

    -- * Destructuring the response
    DeleteTagOptionResponse (..),
    mkDeleteTagOptionResponse,

    -- ** Response lenses
    dtorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeleteTagOption' smart constructor.
newtype DeleteTagOption = DeleteTagOption'
  { -- | The TagOption identifier.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagOption' with the minimum fields required to make a request.
--
-- * 'id' - The TagOption identifier.
mkDeleteTagOption ::
  -- | 'id'
  Lude.Text ->
  DeleteTagOption
mkDeleteTagOption pId_ = DeleteTagOption' {id = pId_}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofId :: Lens.Lens' DeleteTagOption Lude.Text
dtofId = Lens.lens (id :: DeleteTagOption -> Lude.Text) (\s a -> s {id = a} :: DeleteTagOption)
{-# DEPRECATED dtofId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteTagOption where
  type Rs DeleteTagOption = DeleteTagOptionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTagOptionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTagOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.DeleteTagOption" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTagOption where
  toJSON DeleteTagOption' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath DeleteTagOption where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTagOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTagOptionResponse' smart constructor.
newtype DeleteTagOptionResponse = DeleteTagOptionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagOptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTagOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTagOptionResponse
mkDeleteTagOptionResponse pResponseStatus_ =
  DeleteTagOptionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorsResponseStatus :: Lens.Lens' DeleteTagOptionResponse Lude.Int
dtorsResponseStatus = Lens.lens (responseStatus :: DeleteTagOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagOptionResponse)
{-# DEPRECATED dtorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
