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
-- Module      : Network.AWS.MediaLive.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags for a resource
module Network.AWS.MediaLive.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_tagKeys,
    deleteTags_resourceArn,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteTagsRequest
--
-- /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | An array of tag keys to delete
    tagKeys :: [Core.Text],
    resourceArn :: Core.Text
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
-- 'tagKeys', 'deleteTags_tagKeys' - An array of tag keys to delete
--
-- 'resourceArn', 'deleteTags_resourceArn' - Undocumented member.
newDeleteTags ::
  -- | 'resourceArn'
  Core.Text ->
  DeleteTags
newDeleteTags pResourceArn_ =
  DeleteTags'
    { tagKeys = Core.mempty,
      resourceArn = pResourceArn_
    }

-- | An array of tag keys to delete
deleteTags_tagKeys :: Lens.Lens' DeleteTags [Core.Text]
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Core.. Lens._Coerce

-- | Undocumented member.
deleteTags_resourceArn :: Lens.Lens' DeleteTags Core.Text
deleteTags_resourceArn = Lens.lens (\DeleteTags' {resourceArn} -> resourceArn) (\s@DeleteTags' {} a -> s {resourceArn = a} :: DeleteTags)

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteTagsResponse'

instance Core.Hashable DeleteTags

instance Core.NFData DeleteTags

instance Core.ToHeaders DeleteTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteTags where
  toPath DeleteTags' {..} =
    Core.mconcat ["/prod/tags/", Core.toBS resourceArn]

instance Core.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Core.mconcat
      ["tagKeys" Core.=: Core.toQueryList "member" tagKeys]

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
