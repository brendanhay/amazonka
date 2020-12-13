{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified crawler from the AWS Glue Data Catalog, unless the crawler state is @RUNNING@ .
module Network.AWS.Glue.DeleteCrawler
  ( -- * Creating a request
    DeleteCrawler (..),
    mkDeleteCrawler,

    -- ** Request lenses
    dcName,

    -- * Destructuring the response
    DeleteCrawlerResponse (..),
    mkDeleteCrawlerResponse,

    -- ** Response lenses
    dcfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCrawler' smart constructor.
newtype DeleteCrawler = DeleteCrawler'
  { -- | The name of the crawler to remove.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCrawler' with the minimum fields required to make a request.
--
-- * 'name' - The name of the crawler to remove.
mkDeleteCrawler ::
  -- | 'name'
  Lude.Text ->
  DeleteCrawler
mkDeleteCrawler pName_ = DeleteCrawler' {name = pName_}

-- | The name of the crawler to remove.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeleteCrawler Lude.Text
dcName = Lens.lens (name :: DeleteCrawler -> Lude.Text) (\s a -> s {name = a} :: DeleteCrawler)
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteCrawler where
  type Rs DeleteCrawler = DeleteCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCrawlerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCrawler where
  toJSON DeleteCrawler' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCrawlerResponse' smart constructor.
newtype DeleteCrawlerResponse = DeleteCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCrawlerResponse
mkDeleteCrawlerResponse pResponseStatus_ =
  DeleteCrawlerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsResponseStatus :: Lens.Lens' DeleteCrawlerResponse Lude.Int
dcfrsResponseStatus = Lens.lens (responseStatus :: DeleteCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCrawlerResponse)
{-# DEPRECATED dcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
