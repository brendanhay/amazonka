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
-- Module      : Amazonka.CloudWatchLogs.TagLogGroup
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
--
-- CloudWatch Logs doesn’t support IAM policies that prevent users from
-- assigning specified tags to log groups using the
-- @aws:Resource\/key-name @ or @aws:TagKeys@ condition keys. For more
-- information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
module Amazonka.CloudWatchLogs.TagLogGroup
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagLogGroup' smart constructor.
data TagLogGroup = TagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The key-value pairs to use for the tags.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  TagLogGroup
newTagLogGroup pLogGroupName_ =
  TagLogGroup'
    { logGroupName = pLogGroupName_,
      tags = Prelude.mempty
    }

-- | The name of the log group.
tagLogGroup_logGroupName :: Lens.Lens' TagLogGroup Prelude.Text
tagLogGroup_logGroupName = Lens.lens (\TagLogGroup' {logGroupName} -> logGroupName) (\s@TagLogGroup' {} a -> s {logGroupName = a} :: TagLogGroup)

-- | The key-value pairs to use for the tags.
tagLogGroup_tags :: Lens.Lens' TagLogGroup (Prelude.HashMap Prelude.Text Prelude.Text)
tagLogGroup_tags = Lens.lens (\TagLogGroup' {tags} -> tags) (\s@TagLogGroup' {} a -> s {tags = a} :: TagLogGroup) Prelude.. Lens.coerced

instance Core.AWSRequest TagLogGroup where
  type AWSResponse TagLogGroup = TagLogGroupResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagLogGroupResponse'

instance Prelude.Hashable TagLogGroup where
  hashWithSalt salt' TagLogGroup' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData TagLogGroup where
  rnf TagLogGroup' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders TagLogGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.TagLogGroup" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TagLogGroup where
  toJSON TagLogGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Core..= logGroupName),
            Prelude.Just ("tags" Core..= tags)
          ]
      )

instance Core.ToPath TagLogGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery TagLogGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagLogGroupResponse' smart constructor.
data TagLogGroupResponse = TagLogGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagLogGroupResponse ::
  TagLogGroupResponse
newTagLogGroupResponse = TagLogGroupResponse'

instance Prelude.NFData TagLogGroupResponse where
  rnf _ = ()
