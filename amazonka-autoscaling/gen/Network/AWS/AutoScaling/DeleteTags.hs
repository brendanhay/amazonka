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
-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags.
module Network.AWS.AutoScaling.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_tags,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | One or more tags.
    tags :: [Tag]
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
-- 'tags', 'deleteTags_tags' - One or more tags.
newDeleteTags ::
  DeleteTags
newDeleteTags = DeleteTags' {tags = Core.mempty}

-- | One or more tags.
deleteTags_tags :: Lens.Lens' DeleteTags [Tag]
deleteTags_tags = Lens.lens (\DeleteTags' {tags} -> tags) (\s@DeleteTags' {} a -> s {tags = a} :: DeleteTags) Core.. Lens._Coerce

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
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "Tags" Core.=: Core.toQueryList "member" tags
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
