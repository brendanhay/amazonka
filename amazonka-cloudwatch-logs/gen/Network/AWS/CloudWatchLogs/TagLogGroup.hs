{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TagLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the specified tags for the specified log group.
--
-- To list the tags for a log group, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup>.
-- To remove tags, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_UntagLogGroup.html UntagLogGroup>.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Working-with-log-groups-and-streams.html#log-group-tagging Tag Log Groups in Amazon CloudWatch Logs>
-- in the /Amazon CloudWatch Logs User Guide/.
module Network.AWS.CloudWatchLogs.TagLogGroup
  ( -- * Creating a Request
    TagLogGroup (..),
    newTagLogGroup,

    -- * Request Lenses
    tagLogGroup_logGroupName,
    tagLogGroup_tags,

    -- * Destructuring the Response
    TagLogGroupResponse (..),
    newTagLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagLogGroup' smart constructor.
data TagLogGroup = TagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The key-value pairs to use for the tags.
    tags :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'tagLogGroup_logGroupName' - The name of the log group.
--
-- 'tags', 'tagLogGroup_tags' - The key-value pairs to use for the tags.
newTagLogGroup ::
  -- | 'logGroupName'
  Core.Text ->
  TagLogGroup
newTagLogGroup pLogGroupName_ =
  TagLogGroup'
    { logGroupName = pLogGroupName_,
      tags = Core.mempty
    }

-- | The name of the log group.
tagLogGroup_logGroupName :: Lens.Lens' TagLogGroup Core.Text
tagLogGroup_logGroupName = Lens.lens (\TagLogGroup' {logGroupName} -> logGroupName) (\s@TagLogGroup' {} a -> s {logGroupName = a} :: TagLogGroup)

-- | The key-value pairs to use for the tags.
tagLogGroup_tags :: Lens.Lens' TagLogGroup (Core.HashMap Core.Text Core.Text)
tagLogGroup_tags = Lens.lens (\TagLogGroup' {tags} -> tags) (\s@TagLogGroup' {} a -> s {tags = a} :: TagLogGroup) Core.. Lens._Coerce

instance Core.AWSRequest TagLogGroup where
  type AWSResponse TagLogGroup = TagLogGroupResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagLogGroupResponse'

instance Core.Hashable TagLogGroup

instance Core.NFData TagLogGroup

instance Core.ToHeaders TagLogGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.TagLogGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TagLogGroup where
  toJSON TagLogGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("tags" Core..= tags)
          ]
      )

instance Core.ToPath TagLogGroup where
  toPath = Core.const "/"

instance Core.ToQuery TagLogGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTagLogGroupResponse' smart constructor.
data TagLogGroupResponse = TagLogGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagLogGroupResponse ::
  TagLogGroupResponse
newTagLogGroupResponse = TagLogGroupResponse'

instance Core.NFData TagLogGroupResponse
