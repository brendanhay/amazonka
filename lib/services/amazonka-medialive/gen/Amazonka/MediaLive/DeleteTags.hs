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
-- Module      : Amazonka.MediaLive.DeleteTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags for a resource
module Amazonka.MediaLive.DeleteTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DeleteTagsRequest
--
-- /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | An array of tag keys to delete
    tagKeys :: [Prelude.Text],
    resourceArn :: Prelude.Text
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
-- 'tagKeys', 'deleteTags_tagKeys' - An array of tag keys to delete
--
-- 'resourceArn', 'deleteTags_resourceArn' - Undocumented member.
newDeleteTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteTags
newDeleteTags pResourceArn_ =
  DeleteTags'
    { tagKeys = Prelude.mempty,
      resourceArn = pResourceArn_
    }

-- | An array of tag keys to delete
deleteTags_tagKeys :: Lens.Lens' DeleteTags [Prelude.Text]
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Prelude.. Lens.coerced

-- | Undocumented member.
deleteTags_resourceArn :: Lens.Lens' DeleteTags Prelude.Text
deleteTags_resourceArn = Lens.lens (\DeleteTags' {resourceArn} -> resourceArn) (\s@DeleteTags' {} a -> s {resourceArn = a} :: DeleteTags)

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteTagsResponse'

instance Prelude.Hashable DeleteTags where
  hashWithSalt _salt DeleteTags' {..} =
    _salt
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteTags where
  rnf DeleteTags' {..} =
    Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders DeleteTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTags where
  toPath DeleteTags' {..} =
    Prelude.mconcat
      ["/prod/tags/", Data.toBS resourceArn]

instance Data.ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    Prelude.mconcat
      ["tagKeys" Data.=: Data.toQueryList "member" tagKeys]

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
