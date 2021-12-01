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
-- Module      : Amazonka.OpenSearch.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches tags to an existing domain. Tags are a set of case-sensitive
-- key value pairs. An domain can have up to 10 tags. See
-- <http://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-awsresorcetagging Tagging Amazon OpenSearch Service domains>
-- for more information.
module Amazonka.OpenSearch.AddTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ AddTags @ operation. Specifies the
-- tags to attach to the domain.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | Specify the @ARN@ of the domain you want to add tags to.
    arn :: Prelude.Text,
    -- | List of @Tag@ to add to the domain.
    tagList :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'addTags_arn' - Specify the @ARN@ of the domain you want to add tags to.
--
-- 'tagList', 'addTags_tagList' - List of @Tag@ to add to the domain.
newAddTags ::
  -- | 'arn'
  Prelude.Text ->
  AddTags
newAddTags pARN_ =
  AddTags' {arn = pARN_, tagList = Prelude.mempty}

-- | Specify the @ARN@ of the domain you want to add tags to.
addTags_arn :: Lens.Lens' AddTags Prelude.Text
addTags_arn = Lens.lens (\AddTags' {arn} -> arn) (\s@AddTags' {} a -> s {arn = a} :: AddTags)

-- | List of @Tag@ to add to the domain.
addTags_tagList :: Lens.Lens' AddTags [Tag]
addTags_tagList = Lens.lens (\AddTags' {tagList} -> tagList) (\s@AddTags' {} a -> s {tagList = a} :: AddTags) Prelude.. Lens.coerced

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull AddTagsResponse'

instance Prelude.Hashable AddTags where
  hashWithSalt salt' AddTags' {..} =
    salt' `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` arn

instance Prelude.NFData AddTags where
  rnf AddTags' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tagList

instance Core.ToHeaders AddTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON AddTags where
  toJSON AddTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ARN" Core..= arn),
            Prelude.Just ("TagList" Core..= tagList)
          ]
      )

instance Core.ToPath AddTags where
  toPath = Prelude.const "/2021-01-01/tags"

instance Core.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsResponse ::
  AddTagsResponse
newAddTagsResponse = AddTagsResponse'

instance Prelude.NFData AddTagsResponse where
  rnf _ = ()
