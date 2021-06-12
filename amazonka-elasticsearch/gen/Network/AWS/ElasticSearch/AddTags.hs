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
-- Module      : Network.AWS.ElasticSearch.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches tags to an existing Elasticsearch domain. Tags are a set of
-- case-sensitive key value pairs. An Elasticsearch domain may have up to
-- 10 tags. See
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging Tagging Amazon Elasticsearch Service Domains for more information.>
module Network.AWS.ElasticSearch.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_arn,
    addTags_tagList,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @AddTags@ operation. Specify the
-- tags that you want to attach to the Elasticsearch domain.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | Specify the @ARN@ for which you want to add the tags.
    arn :: Core.Text,
    -- | List of @Tag@ that need to be added for the Elasticsearch domain.
    tagList :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'addTags_arn' - Specify the @ARN@ for which you want to add the tags.
--
-- 'tagList', 'addTags_tagList' - List of @Tag@ that need to be added for the Elasticsearch domain.
newAddTags ::
  -- | 'arn'
  Core.Text ->
  AddTags
newAddTags pARN_ =
  AddTags' {arn = pARN_, tagList = Core.mempty}

-- | Specify the @ARN@ for which you want to add the tags.
addTags_arn :: Lens.Lens' AddTags Core.Text
addTags_arn = Lens.lens (\AddTags' {arn} -> arn) (\s@AddTags' {} a -> s {arn = a} :: AddTags)

-- | List of @Tag@ that need to be added for the Elasticsearch domain.
addTags_tagList :: Lens.Lens' AddTags [Tag]
addTags_tagList = Lens.lens (\AddTags' {tagList} -> tagList) (\s@AddTags' {} a -> s {tagList = a} :: AddTags) Core.. Lens._Coerce

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull AddTagsResponse'

instance Core.Hashable AddTags

instance Core.NFData AddTags

instance Core.ToHeaders AddTags where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON AddTags where
  toJSON AddTags' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ARN" Core..= arn),
            Core.Just ("TagList" Core..= tagList)
          ]
      )

instance Core.ToPath AddTags where
  toPath = Core.const "/2015-01-01/tags"

instance Core.ToQuery AddTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsResponse ::
  AddTagsResponse
newAddTagsResponse = AddTagsResponse'

instance Core.NFData AddTagsResponse
