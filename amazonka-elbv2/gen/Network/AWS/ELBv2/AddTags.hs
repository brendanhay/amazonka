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
-- Module      : Network.AWS.ELBv2.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified Elastic Load Balancing
-- resource. You can tag your Application Load Balancers, Network Load
-- Balancers, Gateway Load Balancers, target groups, listeners, and rules.
--
-- Each tag consists of a key and an optional value. If a resource already
-- has a tag with the same key, @AddTags@ updates its value.
module Network.AWS.ELBv2.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_resourceArns,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArns :: [Prelude.Text],
    -- | The tags.
    tags :: Prelude.NonEmpty Tag
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
-- 'resourceArns', 'addTags_resourceArns' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tags', 'addTags_tags' - The tags.
newAddTags ::
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  AddTags
newAddTags pTags_ =
  AddTags'
    { resourceArns = Prelude.mempty,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | The Amazon Resource Name (ARN) of the resource.
addTags_resourceArns :: Lens.Lens' AddTags [Prelude.Text]
addTags_resourceArns = Lens.lens (\AddTags' {resourceArns} -> resourceArns) (\s@AddTags' {} a -> s {resourceArns = a} :: AddTags) Prelude.. Lens._Coerce

-- | The tags.
addTags_tags :: Lens.Lens' AddTags (Prelude.NonEmpty Tag)
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Lens._Coerce

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AddTagsResult"
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags

instance Prelude.NFData AddTags

instance Core.ToHeaders AddTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AddTags where
  toPath = Prelude.const "/"

instance Core.ToQuery AddTags where
  toQuery AddTags' {..} =
    Prelude.mconcat
      [ "Action" Core.=: ("AddTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "ResourceArns"
          Core.=: Core.toQueryList "member" resourceArns,
        "Tags" Core.=: Core.toQueryList "member" tags
      ]

-- | /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse
