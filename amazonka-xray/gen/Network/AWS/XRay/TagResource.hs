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
-- Module      : Network.AWS.XRay.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies tags to an existing AWS X-Ray group or sampling rule.
module Network.AWS.XRay.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceARN,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
    resourceARN :: Prelude.Text,
    -- | A map that contains one or more tag keys and tag values to attach to an
    -- X-Ray group or sampling rule. For more information about ways to use
    -- tags, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference/.
    --
    -- The following restrictions apply to tags:
    --
    -- -   Maximum number of user-applied tags per resource: 50
    --
    -- -   Maximum tag key length: 128 Unicode characters
    --
    -- -   Maximum tag value length: 256 Unicode characters
    --
    -- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
    --     following characters: _ . : \/ = + - and \@
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
    --     You cannot edit or delete system tags.
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
-- 'resourceARN', 'tagResource_resourceARN' - The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
--
-- 'tags', 'tagResource_tags' - A map that contains one or more tag keys and tag values to attach to an
-- X-Ray group or sampling rule. For more information about ways to use
-- tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
--     You cannot edit or delete system tags.
newTagResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  TagResource
newTagResource pResourceARN_ =
  TagResource'
    { resourceARN = pResourceARN_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
tagResource_resourceARN :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceARN = Lens.lens (\TagResource' {resourceARN} -> resourceARN) (\s@TagResource' {} a -> s {resourceARN = a} :: TagResource)

-- | A map that contains one or more tag keys and tag values to attach to an
-- X-Ray group or sampling rule. For more information about ways to use
-- tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
--     You cannot edit or delete system tags.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens._Coerce

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Core.ToHeaders TagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON TagResource where
  toJSON TagResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Core..= resourceARN),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagResource where
  toPath = Prelude.const "/TagResource"

instance Core.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'tagResourceResponse_httpStatus' - The response's http status code.
newTagResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagResourceResponse
newTagResourceResponse pHttpStatus_ =
  TagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
tagResourceResponse_httpStatus :: Lens.Lens' TagResourceResponse Prelude.Int
tagResourceResponse_httpStatus = Lens.lens (\TagResourceResponse' {httpStatus} -> httpStatus) (\s@TagResourceResponse' {} a -> s {httpStatus = a} :: TagResourceResponse)

instance Prelude.NFData TagResourceResponse
