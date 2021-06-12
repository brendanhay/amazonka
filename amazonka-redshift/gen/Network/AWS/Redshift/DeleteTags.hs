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
-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a resource. You must provide the ARN of the resource
-- from which you want to delete the tag or tags.
module Network.AWS.Redshift.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_resourceName,
    deleteTags_tagKeys,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the output from the @DeleteTags@ action.
--
-- /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The Amazon Resource Name (ARN) from which you want to remove the tag or
    -- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
    resourceName :: Core.Text,
    -- | The tag key that you want to delete.
    tagKeys :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'deleteTags_resourceName' - The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
--
-- 'tagKeys', 'deleteTags_tagKeys' - The tag key that you want to delete.
newDeleteTags ::
  -- | 'resourceName'
  Core.Text ->
  DeleteTags
newDeleteTags pResourceName_ =
  DeleteTags'
    { resourceName = pResourceName_,
      tagKeys = Core.mempty
    }

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
deleteTags_resourceName :: Lens.Lens' DeleteTags Core.Text
deleteTags_resourceName = Lens.lens (\DeleteTags' {resourceName} -> resourceName) (\s@DeleteTags' {} a -> s {resourceName = a} :: DeleteTags)

-- | The tag key that you want to delete.
deleteTags_tagKeys :: Lens.Lens' DeleteTags [Core.Text]
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Core.. Lens._Coerce

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteTagsResponse'

instance Core.Hashable DeleteTags

instance Core.NFData DeleteTags

instance Core.ToHeaders DeleteTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTags where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Core.mconcat
      [ "Action" Core.=: ("DeleteTags" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ResourceName" Core.=: resourceName,
        "TagKeys" Core.=: Core.toQueryList "TagKey" tagKeys
      ]

-- | /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTagsResponse ::
  DeleteTagsResponse
newDeleteTagsResponse = DeleteTagsResponse'

instance Core.NFData DeleteTagsResponse
