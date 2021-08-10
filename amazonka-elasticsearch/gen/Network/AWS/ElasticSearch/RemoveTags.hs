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
-- Module      : Network.AWS.ElasticSearch.RemoveTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified set of tags from the specified Elasticsearch
-- domain.
module Network.AWS.ElasticSearch.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_arn,
    removeTags_tagKeys,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @RemoveTags@ operation. Specify the
-- @ARN@ for the Elasticsearch domain from which you want to remove the
-- specified @TagKey@.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | Specifies the @ARN@ for the Elasticsearch domain from which you want to
    -- delete the specified tags.
    arn :: Prelude.Text,
    -- | Specifies the @TagKey@ list which you want to remove from the
    -- Elasticsearch domain.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'removeTags_arn' - Specifies the @ARN@ for the Elasticsearch domain from which you want to
-- delete the specified tags.
--
-- 'tagKeys', 'removeTags_tagKeys' - Specifies the @TagKey@ list which you want to remove from the
-- Elasticsearch domain.
newRemoveTags ::
  -- | 'arn'
  Prelude.Text ->
  RemoveTags
newRemoveTags pARN_ =
  RemoveTags' {arn = pARN_, tagKeys = Prelude.mempty}

-- | Specifies the @ARN@ for the Elasticsearch domain from which you want to
-- delete the specified tags.
removeTags_arn :: Lens.Lens' RemoveTags Prelude.Text
removeTags_arn = Lens.lens (\RemoveTags' {arn} -> arn) (\s@RemoveTags' {} a -> s {arn = a} :: RemoveTags)

-- | Specifies the @TagKey@ list which you want to remove from the
-- Elasticsearch domain.
removeTags_tagKeys :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_tagKeys = Lens.lens (\RemoveTags' {tagKeys} -> tagKeys) (\s@RemoveTags' {} a -> s {tagKeys = a} :: RemoveTags) Prelude.. Lens._Coerce

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull RemoveTagsResponse'

instance Prelude.Hashable RemoveTags

instance Prelude.NFData RemoveTags

instance Core.ToHeaders RemoveTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ARN" Core..= arn),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath RemoveTags where
  toPath = Prelude.const "/2015-01-01/tags-removal"

instance Core.ToQuery RemoveTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsResponse ::
  RemoveTagsResponse
newRemoveTagsResponse = RemoveTagsResponse'

instance Prelude.NFData RemoveTagsResponse
