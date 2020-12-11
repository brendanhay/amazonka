{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.UntagLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified log group.
--
-- To list the tags for a log group, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup> . To add tags, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TagLogGroup.html TagLogGroup> .
module Network.AWS.CloudWatchLogs.UntagLogGroup
  ( -- * Creating a request
    UntagLogGroup (..),
    mkUntagLogGroup,

    -- ** Request lenses
    ulgLogGroupName,
    ulgTags,

    -- * Destructuring the response
    UntagLogGroupResponse (..),
    mkUntagLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagLogGroup' smart constructor.
data UntagLogGroup = UntagLogGroup'
  { logGroupName :: Lude.Text,
    tags :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'tags' - The tag keys. The corresponding tags are removed from the log group.
mkUntagLogGroup ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Lude.Text ->
  UntagLogGroup
mkUntagLogGroup pLogGroupName_ pTags_ =
  UntagLogGroup' {logGroupName = pLogGroupName_, tags = pTags_}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulgLogGroupName :: Lens.Lens' UntagLogGroup Lude.Text
ulgLogGroupName = Lens.lens (logGroupName :: UntagLogGroup -> Lude.Text) (\s a -> s {logGroupName = a} :: UntagLogGroup)
{-# DEPRECATED ulgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The tag keys. The corresponding tags are removed from the log group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulgTags :: Lens.Lens' UntagLogGroup (Lude.NonEmpty Lude.Text)
ulgTags = Lens.lens (tags :: UntagLogGroup -> Lude.NonEmpty Lude.Text) (\s a -> s {tags = a} :: UntagLogGroup)
{-# DEPRECATED ulgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest UntagLogGroup where
  type Rs UntagLogGroup = UntagLogGroupResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull UntagLogGroupResponse'

instance Lude.ToHeaders UntagLogGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.UntagLogGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagLogGroup where
  toJSON UntagLogGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("tags" Lude..= tags)
          ]
      )

instance Lude.ToPath UntagLogGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagLogGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagLogGroupResponse' smart constructor.
data UntagLogGroupResponse = UntagLogGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagLogGroupResponse' with the minimum fields required to make a request.
mkUntagLogGroupResponse ::
  UntagLogGroupResponse
mkUntagLogGroupResponse = UntagLogGroupResponse'
