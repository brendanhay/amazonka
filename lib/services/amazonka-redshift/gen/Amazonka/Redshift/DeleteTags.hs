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
-- Module      : Amazonka.Redshift.DeleteTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a resource. You must provide the ARN of the resource
-- from which you want to delete the tag or tags.
module Amazonka.Redshift.DeleteTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the output from the @DeleteTags@ action.
--
-- /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The Amazon Resource Name (ARN) from which you want to remove the tag or
    -- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
    resourceName :: Prelude.Text,
    -- | The tag key that you want to delete.
    tagKeys :: [Prelude.Text]
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
-- 'resourceName', 'deleteTags_resourceName' - The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
--
-- 'tagKeys', 'deleteTags_tagKeys' - The tag key that you want to delete.
newDeleteTags ::
  -- | 'resourceName'
  Prelude.Text ->
  DeleteTags
newDeleteTags pResourceName_ =
  DeleteTags'
    { resourceName = pResourceName_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
deleteTags_resourceName :: Lens.Lens' DeleteTags Prelude.Text
deleteTags_resourceName = Lens.lens (\DeleteTags' {resourceName} -> resourceName) (\s@DeleteTags' {} a -> s {resourceName = a} :: DeleteTags)

-- | The tag key that you want to delete.
deleteTags_tagKeys :: Lens.Lens' DeleteTags [Prelude.Text]
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull DeleteTagsResponse'

instance Prelude.Hashable DeleteTags where
  hashWithSalt _salt DeleteTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData DeleteTags where
  rnf DeleteTags' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf tagKeys

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
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ResourceName" Data.=: resourceName,
        "TagKeys" Data.=: Data.toQueryList "TagKey" tagKeys
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
