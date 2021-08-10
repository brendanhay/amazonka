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
-- Module      : Network.AWS.ResourceGroups.GetTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are associated with a resource group,
-- specified by an ARN.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:GetTags@
module Network.AWS.ResourceGroups.GetTags
  ( -- * Creating a Request
    GetTags (..),
    newGetTags,

    -- * Request Lenses
    getTags_arn,

    -- * Destructuring the Response
    GetTagsResponse (..),
    newGetTagsResponse,

    -- * Response Lenses
    getTagsResponse_arn,
    getTagsResponse_tags,
    getTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTags' smart constructor.
data GetTags = GetTags'
  { -- | The ARN of the resource group whose tags you want to retrieve.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTags_arn' - The ARN of the resource group whose tags you want to retrieve.
newGetTags ::
  -- | 'arn'
  Prelude.Text ->
  GetTags
newGetTags pArn_ = GetTags' {arn = pArn_}

-- | The ARN of the resource group whose tags you want to retrieve.
getTags_arn :: Lens.Lens' GetTags Prelude.Text
getTags_arn = Lens.lens (\GetTags' {arn} -> arn) (\s@GetTags' {} a -> s {arn = a} :: GetTags)

instance Core.AWSRequest GetTags where
  type AWSResponse GetTags = GetTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTags

instance Prelude.NFData GetTags

instance Core.ToHeaders GetTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetTags where
  toPath GetTags' {..} =
    Prelude.mconcat
      ["/resources/", Core.toBS arn, "/tags"]

instance Core.ToQuery GetTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The ARN of the tagged resource group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the specified resource group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTagsResponse_arn' - The ARN of the tagged resource group.
--
-- 'tags', 'getTagsResponse_tags' - The tags associated with the specified resource group.
--
-- 'httpStatus', 'getTagsResponse_httpStatus' - The response's http status code.
newGetTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTagsResponse
newGetTagsResponse pHttpStatus_ =
  GetTagsResponse'
    { arn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the tagged resource group.
getTagsResponse_arn :: Lens.Lens' GetTagsResponse (Prelude.Maybe Prelude.Text)
getTagsResponse_arn = Lens.lens (\GetTagsResponse' {arn} -> arn) (\s@GetTagsResponse' {} a -> s {arn = a} :: GetTagsResponse)

-- | The tags associated with the specified resource group.
getTagsResponse_tags :: Lens.Lens' GetTagsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getTagsResponse_tags = Lens.lens (\GetTagsResponse' {tags} -> tags) (\s@GetTagsResponse' {} a -> s {tags = a} :: GetTagsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTagsResponse_httpStatus :: Lens.Lens' GetTagsResponse Prelude.Int
getTagsResponse_httpStatus = Lens.lens (\GetTagsResponse' {httpStatus} -> httpStatus) (\s@GetTagsResponse' {} a -> s {httpStatus = a} :: GetTagsResponse)

instance Prelude.NFData GetTagsResponse
