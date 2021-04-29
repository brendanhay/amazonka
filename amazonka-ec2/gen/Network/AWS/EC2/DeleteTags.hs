{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
--
-- To list the current tags, use DescribeTags. For more information about
-- tags, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_dryRun,
    deleteTags_tags,
    deleteTags_resources,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to delete. Specify a tag key and an optional tag value to
    -- delete specific tags. If you specify a tag key without a tag value, we
    -- delete any tag with this key regardless of its value. If you specify a
    -- tag key with an empty string as the tag value, we delete the tag only if
    -- its value is an empty string.
    --
    -- If you omit this parameter, we delete all user-defined tags for the
    -- specified resources. We do not delete AWS-generated tags (tags that have
    -- the @aws:@ prefix).
    tags :: Prelude.Maybe [Tag],
    -- | The IDs of the resources, separated by spaces.
    --
    -- Constraints: Up to 1000 resource IDs. We recommend breaking up this
    -- request into smaller batches.
    resources :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTags_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tags', 'deleteTags_tags' - The tags to delete. Specify a tag key and an optional tag value to
-- delete specific tags. If you specify a tag key without a tag value, we
-- delete any tag with this key regardless of its value. If you specify a
-- tag key with an empty string as the tag value, we delete the tag only if
-- its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the
-- specified resources. We do not delete AWS-generated tags (tags that have
-- the @aws:@ prefix).
--
-- 'resources', 'deleteTags_resources' - The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
newDeleteTags ::
  DeleteTags
newDeleteTags =
  DeleteTags'
    { dryRun = Prelude.Nothing,
      tags = Prelude.Nothing,
      resources = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTags_dryRun :: Lens.Lens' DeleteTags (Prelude.Maybe Prelude.Bool)
deleteTags_dryRun = Lens.lens (\DeleteTags' {dryRun} -> dryRun) (\s@DeleteTags' {} a -> s {dryRun = a} :: DeleteTags)

-- | The tags to delete. Specify a tag key and an optional tag value to
-- delete specific tags. If you specify a tag key without a tag value, we
-- delete any tag with this key regardless of its value. If you specify a
-- tag key with an empty string as the tag value, we delete the tag only if
-- its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the
-- specified resources. We do not delete AWS-generated tags (tags that have
-- the @aws:@ prefix).
deleteTags_tags :: Lens.Lens' DeleteTags (Prelude.Maybe [Tag])
deleteTags_tags = Lens.lens (\DeleteTags' {tags} -> tags) (\s@DeleteTags' {} a -> s {tags = a} :: DeleteTags) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
deleteTags_resources :: Lens.Lens' DeleteTags [Prelude.Text]
deleteTags_resources = Lens.lens (\DeleteTags' {resources} -> resources) (\s@DeleteTags' {} a -> s {resources = a} :: DeleteTags) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteTagsResponse'

instance Prelude.Hashable DeleteTags

instance Prelude.NFData DeleteTags

instance Prelude.ToHeaders DeleteTags where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteTags" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          (Prelude.toQueryList "Tag" Prelude.<$> tags),
        Prelude.toQueryList "ResourceId" resources
      ]

-- | /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTagsResponse ::
  DeleteTagsResponse
newDeleteTagsResponse = DeleteTagsResponse'

instance Prelude.NFData DeleteTagsResponse
