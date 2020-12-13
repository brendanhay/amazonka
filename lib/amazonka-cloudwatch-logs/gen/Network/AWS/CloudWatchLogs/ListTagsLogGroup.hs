{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.ListTagsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified log group.
module Network.AWS.CloudWatchLogs.ListTagsLogGroup
  ( -- * Creating a request
    ListTagsLogGroup (..),
    mkListTagsLogGroup,

    -- ** Request lenses
    ltlgLogGroupName,

    -- * Destructuring the response
    ListTagsLogGroupResponse (..),
    mkListTagsLogGroupResponse,

    -- ** Response lenses
    ltlgrsTags,
    ltlgrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsLogGroup' smart constructor.
newtype ListTagsLogGroup = ListTagsLogGroup'
  { -- | The name of the log group.
    logGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
mkListTagsLogGroup ::
  -- | 'logGroupName'
  Lude.Text ->
  ListTagsLogGroup
mkListTagsLogGroup pLogGroupName_ =
  ListTagsLogGroup' {logGroupName = pLogGroupName_}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgLogGroupName :: Lens.Lens' ListTagsLogGroup Lude.Text
ltlgLogGroupName = Lens.lens (logGroupName :: ListTagsLogGroup -> Lude.Text) (\s a -> s {logGroupName = a} :: ListTagsLogGroup)
{-# DEPRECATED ltlgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest ListTagsLogGroup where
  type Rs ListTagsLogGroup = ListTagsLogGroupResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsLogGroupResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsLogGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.ListTagsLogGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsLogGroup where
  toJSON ListTagsLogGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("logGroupName" Lude..= logGroupName)])

instance Lude.ToPath ListTagsLogGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsLogGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsLogGroupResponse' smart constructor.
data ListTagsLogGroupResponse = ListTagsLogGroupResponse'
  { -- | The tags for the log group.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsLogGroupResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The tags for the log group.
-- * 'responseStatus' - The response status code.
mkListTagsLogGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsLogGroupResponse
mkListTagsLogGroupResponse pResponseStatus_ =
  ListTagsLogGroupResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tags for the log group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrsTags :: Lens.Lens' ListTagsLogGroupResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltlgrsTags = Lens.lens (tags :: ListTagsLogGroupResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTagsLogGroupResponse)
{-# DEPRECATED ltlgrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlgrsResponseStatus :: Lens.Lens' ListTagsLogGroupResponse Lude.Int
ltlgrsResponseStatus = Lens.lens (responseStatus :: ListTagsLogGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsLogGroupResponse)
{-# DEPRECATED ltlgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
