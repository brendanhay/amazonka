{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TagLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the specified tags for the specified log group.
--
-- To list the tags for a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup> . To remove tags, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_UntagLogGroup.html UntagLogGroup> .
-- For more information about tags, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html#log-group-tagging Tag Log Groups in Amazon CloudWatch Logs> in the /Amazon CloudWatch Logs User Guide/ .
module Network.AWS.CloudWatchLogs.TagLogGroup
  ( -- * Creating a request
    TagLogGroup (..),
    mkTagLogGroup,

    -- ** Request lenses
    tlgLogGroupName,
    tlgTags,

    -- * Destructuring the response
    TagLogGroupResponse (..),
    mkTagLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagLogGroup' smart constructor.
data TagLogGroup = TagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The key-value pairs to use for the tags.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'tags' - The key-value pairs to use for the tags.
mkTagLogGroup ::
  -- | 'logGroupName'
  Lude.Text ->
  TagLogGroup
mkTagLogGroup pLogGroupName_ =
  TagLogGroup' {logGroupName = pLogGroupName_, tags = Lude.mempty}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlgLogGroupName :: Lens.Lens' TagLogGroup Lude.Text
tlgLogGroupName = Lens.lens (logGroupName :: TagLogGroup -> Lude.Text) (\s a -> s {logGroupName = a} :: TagLogGroup)
{-# DEPRECATED tlgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The key-value pairs to use for the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlgTags :: Lens.Lens' TagLogGroup (Lude.HashMap Lude.Text (Lude.Text))
tlgTags = Lens.lens (tags :: TagLogGroup -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagLogGroup)
{-# DEPRECATED tlgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagLogGroup where
  type Rs TagLogGroup = TagLogGroupResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull TagLogGroupResponse'

instance Lude.ToHeaders TagLogGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.TagLogGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagLogGroup where
  toJSON TagLogGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagLogGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery TagLogGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagLogGroupResponse' smart constructor.
data TagLogGroupResponse = TagLogGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagLogGroupResponse' with the minimum fields required to make a request.
mkTagLogGroupResponse ::
  TagLogGroupResponse
mkTagLogGroupResponse = TagLogGroupResponse'
