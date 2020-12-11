{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, returns a <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-exceptions.html#aws-glue-api-exceptions-CrawlerRunningException CrawlerRunningException> .
module Network.AWS.Glue.StartCrawler
  ( -- * Creating a request
    StartCrawler (..),
    mkStartCrawler,

    -- ** Request lenses
    scName,

    -- * Destructuring the response
    StartCrawlerResponse (..),
    mkStartCrawlerResponse,

    -- ** Response lenses
    scrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartCrawler' smart constructor.
newtype StartCrawler = StartCrawler' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartCrawler' with the minimum fields required to make a request.
--
-- * 'name' - Name of the crawler to start.
mkStartCrawler ::
  -- | 'name'
  Lude.Text ->
  StartCrawler
mkStartCrawler pName_ = StartCrawler' {name = pName_}

-- | Name of the crawler to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' StartCrawler Lude.Text
scName = Lens.lens (name :: StartCrawler -> Lude.Text) (\s a -> s {name = a} :: StartCrawler)
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartCrawler where
  type Rs StartCrawler = StartCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartCrawlerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartCrawler where
  toJSON StartCrawler' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery StartCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartCrawlerResponse' smart constructor.
newtype StartCrawlerResponse = StartCrawlerResponse'
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

-- | Creates a value of 'StartCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartCrawlerResponse
mkStartCrawlerResponse pResponseStatus_ =
  StartCrawlerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsResponseStatus :: Lens.Lens' StartCrawlerResponse Lude.Int
scrsResponseStatus = Lens.lens (responseStatus :: StartCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartCrawlerResponse)
{-# DEPRECATED scrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
