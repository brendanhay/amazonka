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
-- Module      : Amazonka.CloudWatchLogs.UntagLogGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
--
-- CloudWatch Logs doesn’t support IAM policies that prevent users from
-- assigning specified tags to log groups using the
-- @aws:Resource\/key-name @ or @aws:TagKeys@ condition keys.
module Amazonka.CloudWatchLogs.UntagLogGroup
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagLogGroup' smart constructor.
data UntagLogGroup = UntagLogGroup'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The tag keys. The corresponding tags are removed from the log group.
    tags :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Prelude.Text ->
  UntagLogGroup
newUntagLogGroup pLogGroupName_ pTags_ =
  UntagLogGroup'
    { logGroupName = pLogGroupName_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The name of the log group.
untagLogGroup_logGroupName :: Lens.Lens' UntagLogGroup Prelude.Text
untagLogGroup_logGroupName = Lens.lens (\UntagLogGroup' {logGroupName} -> logGroupName) (\s@UntagLogGroup' {} a -> s {logGroupName = a} :: UntagLogGroup)

-- | The tag keys. The corresponding tags are removed from the log group.
untagLogGroup_tags :: Lens.Lens' UntagLogGroup (Prelude.NonEmpty Prelude.Text)
untagLogGroup_tags = Lens.lens (\UntagLogGroup' {tags} -> tags) (\s@UntagLogGroup' {} a -> s {tags = a} :: UntagLogGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UntagLogGroup where
  type
    AWSResponse UntagLogGroup =
      UntagLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UntagLogGroupResponse'

instance Prelude.Hashable UntagLogGroup where
  hashWithSalt _salt UntagLogGroup' {..} =
    _salt `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData UntagLogGroup where
  rnf UntagLogGroup' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders UntagLogGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.UntagLogGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UntagLogGroup where
  toJSON UntagLogGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Core..= logGroupName),
            Prelude.Just ("tags" Core..= tags)
          ]
      )

instance Core.ToPath UntagLogGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UntagLogGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagLogGroupResponse' smart constructor.
data UntagLogGroupResponse = UntagLogGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagLogGroupResponse ::
  UntagLogGroupResponse
newUntagLogGroupResponse = UntagLogGroupResponse'

instance Prelude.NFData UntagLogGroupResponse where
  rnf _ = ()
