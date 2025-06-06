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
-- Module      : Amazonka.DocumentDB.RemoveTagsFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an Amazon DocumentDB resource.
module Amazonka.DocumentDB.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to RemoveTagsFromResource.
--
-- /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon DocumentDB resource that the tags are removed from. This
    -- value is an Amazon Resource Name (ARN).
    resourceName :: Prelude.Text,
    -- | The tag key (name) of the tag to be removed.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'removeTagsFromResource_resourceName' - The Amazon DocumentDB resource that the tags are removed from. This
-- value is an Amazon Resource Name (ARN).
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - The tag key (name) of the tag to be removed.
newRemoveTagsFromResource ::
  -- | 'resourceName'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceName_ =
  RemoveTagsFromResource'
    { resourceName =
        pResourceName_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon DocumentDB resource that the tags are removed from. This
-- value is an Amazon Resource Name (ARN).
removeTagsFromResource_resourceName :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceName = Lens.lens (\RemoveTagsFromResource' {resourceName} -> resourceName) (\s@RemoveTagsFromResource' {} a -> s {resourceName = a} :: RemoveTagsFromResource)

-- | The tag key (name) of the tag to be removed.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      RemoveTagsFromResourceResponse'

instance Prelude.Hashable RemoveTagsFromResource where
  hashWithSalt _salt RemoveTagsFromResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromResource where
  rnf RemoveTagsFromResource' {..} =
    Prelude.rnf resourceName `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTagsFromResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromResource where
  toQuery RemoveTagsFromResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemoveTagsFromResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ResourceName" Data.=: resourceName,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromResourceResponse ::
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse =
  RemoveTagsFromResourceResponse'

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
  where
  rnf _ = ()
