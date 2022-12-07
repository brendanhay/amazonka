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
-- Module      : Amazonka.EC2.DeleteTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
--
-- To list the current tags, use DescribeTags. For more information about
-- tags, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tag your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_tags,
    deleteTags_dryRun,
    deleteTags_resources,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The tags to delete. Specify a tag key and an optional tag value to
    -- delete specific tags. If you specify a tag key without a tag value, we
    -- delete any tag with this key regardless of its value. If you specify a
    -- tag key with an empty string as the tag value, we delete the tag only if
    -- its value is an empty string.
    --
    -- If you omit this parameter, we delete all user-defined tags for the
    -- specified resources. We do not delete Amazon Web Services-generated tags
    -- (tags that have the @aws:@ prefix).
    --
    -- Constraints: Up to 1000 tags.
    tags :: Prelude.Maybe [Tag],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the resources, separated by spaces.
    --
    -- Constraints: Up to 1000 resource IDs. We recommend breaking up this
    -- request into smaller batches.
    resources :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'deleteTags_tags' - The tags to delete. Specify a tag key and an optional tag value to
-- delete specific tags. If you specify a tag key without a tag value, we
-- delete any tag with this key regardless of its value. If you specify a
-- tag key with an empty string as the tag value, we delete the tag only if
-- its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the
-- specified resources. We do not delete Amazon Web Services-generated tags
-- (tags that have the @aws:@ prefix).
--
-- Constraints: Up to 1000 tags.
--
-- 'dryRun', 'deleteTags_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'resources', 'deleteTags_resources' - The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
newDeleteTags ::
  DeleteTags
newDeleteTags =
  DeleteTags'
    { tags = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      resources = Prelude.mempty
    }

-- | The tags to delete. Specify a tag key and an optional tag value to
-- delete specific tags. If you specify a tag key without a tag value, we
-- delete any tag with this key regardless of its value. If you specify a
-- tag key with an empty string as the tag value, we delete the tag only if
-- its value is an empty string.
--
-- If you omit this parameter, we delete all user-defined tags for the
-- specified resources. We do not delete Amazon Web Services-generated tags
-- (tags that have the @aws:@ prefix).
--
-- Constraints: Up to 1000 tags.
deleteTags_tags :: Lens.Lens' DeleteTags (Prelude.Maybe [Tag])
deleteTags_tags = Lens.lens (\DeleteTags' {tags} -> tags) (\s@DeleteTags' {} a -> s {tags = a} :: DeleteTags) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTags_dryRun :: Lens.Lens' DeleteTags (Prelude.Maybe Prelude.Bool)
deleteTags_dryRun = Lens.lens (\DeleteTags' {dryRun} -> dryRun) (\s@DeleteTags' {} a -> s {dryRun = a} :: DeleteTags)

-- | The IDs of the resources, separated by spaces.
--
-- Constraints: Up to 1000 resource IDs. We recommend breaking up this
-- request into smaller batches.
deleteTags_resources :: Lens.Lens' DeleteTags [Prelude.Text]
deleteTags_resources = Lens.lens (\DeleteTags' {resources} -> resources) (\s@DeleteTags' {} a -> s {resources = a} :: DeleteTags) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteTagsResponse'

instance Prelude.Hashable DeleteTags where
  hashWithSalt _salt DeleteTags' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` resources

instance Prelude.NFData DeleteTags where
  rnf DeleteTags' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf resources

instance Data.ToHeaders DeleteTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "Tag" Prelude.<$> tags),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "ResourceId" resources
      ]

-- | /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTagsResponse ::
  DeleteTagsResponse
newDeleteTagsResponse = DeleteTagsResponse'

instance Prelude.NFData DeleteTagsResponse where
  rnf _ = ()
