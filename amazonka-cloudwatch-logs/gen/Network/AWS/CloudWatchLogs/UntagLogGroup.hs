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
-- Module      : Network.AWS.CloudWatchLogs.UntagLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified log group.
--
-- To list the tags for a log group, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_ListTagsLogGroup.html ListTagsLogGroup>.
-- To add tags, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TagLogGroup.html TagLogGroup>.
module Network.AWS.CloudWatchLogs.UntagLogGroup
  ( -- * Creating a Request
    UntagLogGroup (..),
    newUntagLogGroup,

    -- * Request Lenses
    untagLogGroup_logGroupName,
    untagLogGroup_tags,

    -- * Destructuring the Response
    UntagLogGroupResponse (..),
    newUntagLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagLogGroup' smart constructor.
data UntagLogGroup = UntagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The tag keys. The corresponding tags are removed from the log group.
    tags :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'untagLogGroup_logGroupName' - The name of the log group.
--
-- 'tags', 'untagLogGroup_tags' - The tag keys. The corresponding tags are removed from the log group.
newUntagLogGroup ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'tags'
  Core.NonEmpty Core.Text ->
  UntagLogGroup
newUntagLogGroup pLogGroupName_ pTags_ =
  UntagLogGroup'
    { logGroupName = pLogGroupName_,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | The name of the log group.
untagLogGroup_logGroupName :: Lens.Lens' UntagLogGroup Core.Text
untagLogGroup_logGroupName = Lens.lens (\UntagLogGroup' {logGroupName} -> logGroupName) (\s@UntagLogGroup' {} a -> s {logGroupName = a} :: UntagLogGroup)

-- | The tag keys. The corresponding tags are removed from the log group.
untagLogGroup_tags :: Lens.Lens' UntagLogGroup (Core.NonEmpty Core.Text)
untagLogGroup_tags = Lens.lens (\UntagLogGroup' {tags} -> tags) (\s@UntagLogGroup' {} a -> s {tags = a} :: UntagLogGroup) Core.. Lens._Coerce

instance Core.AWSRequest UntagLogGroup where
  type
    AWSResponse UntagLogGroup =
      UntagLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UntagLogGroupResponse'

instance Core.Hashable UntagLogGroup

instance Core.NFData UntagLogGroup

instance Core.ToHeaders UntagLogGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.UntagLogGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UntagLogGroup where
  toJSON UntagLogGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("tags" Core..= tags)
          ]
      )

instance Core.ToPath UntagLogGroup where
  toPath = Core.const "/"

instance Core.ToQuery UntagLogGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUntagLogGroupResponse' smart constructor.
data UntagLogGroupResponse = UntagLogGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagLogGroupResponse ::
  UntagLogGroupResponse
newUntagLogGroupResponse = UntagLogGroupResponse'

instance Core.NFData UntagLogGroupResponse
