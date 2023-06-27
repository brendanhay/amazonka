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
-- Module      : Amazonka.Organizations.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to the specified resource.
--
-- Currently, you can attach tags to the following resources in
-- Organizations.
--
-- -   Amazon Web Services account
--
-- -   Organization root
--
-- -   Organizational unit (OU)
--
-- -   Policy (any type)
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceId,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The ID of the resource to add a tag to.
    --
    -- You can specify any of the following taggable resources.
    --
    -- -   Amazon Web Services account – specify the account ID number.
    --
    -- -   Organizational unit – specify the OU ID that begins with @ou-@ and
    --     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
    --
    -- -   Root – specify the root ID that begins with @r-@ and looks similar
    --     to: @r-@/@1a2b@/@ @
    --
    -- -   Policy – specify the policy ID that begins with @p-@ andlooks
    --     similar to: @p-@/@12abcdefg3@/@ @
    resourceId :: Prelude.Text,
    -- | A list of tags to add to the specified resource.
    --
    -- For each tag in the list, you must specify both a tag key and a value.
    -- The value can be an empty string, but you can\'t set it to @null@.
    --
    -- If any one of the tags is not valid or if you exceed the maximum allowed
    -- number of tags for a resource, then the entire request fails.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'tagResource_resourceId' - The ID of the resource to add a tag to.
--
-- You can specify any of the following taggable resources.
--
-- -   Amazon Web Services account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-@/@1a2b@/@ @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-@/@12abcdefg3@/@ @
--
-- 'tags', 'tagResource_tags' - A list of tags to add to the specified resource.
--
-- For each tag in the list, you must specify both a tag key and a value.
-- The value can be an empty string, but you can\'t set it to @null@.
--
-- If any one of the tags is not valid or if you exceed the maximum allowed
-- number of tags for a resource, then the entire request fails.
newTagResource ::
  -- | 'resourceId'
  Prelude.Text ->
  TagResource
newTagResource pResourceId_ =
  TagResource'
    { resourceId = pResourceId_,
      tags = Prelude.mempty
    }

-- | The ID of the resource to add a tag to.
--
-- You can specify any of the following taggable resources.
--
-- -   Amazon Web Services account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-@/@1a2b@/@ @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-@/@12abcdefg3@/@ @
tagResource_resourceId :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceId = Lens.lens (\TagResource' {resourceId} -> resourceId) (\s@TagResource' {} a -> s {resourceId = a} :: TagResource)

-- | A list of tags to add to the specified resource.
--
-- For each tag in the list, you must specify both a tag key and a value.
-- The value can be an empty string, but you can\'t set it to @null@.
--
-- If any one of the tags is not valid or if you exceed the maximum allowed
-- number of tags for a resource, then the entire request fails.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource where
  hashWithSalt _salt TagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.TagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TagResource where
  toJSON TagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse where
  rnf _ = ()
