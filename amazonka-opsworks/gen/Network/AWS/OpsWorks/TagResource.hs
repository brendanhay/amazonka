{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply cost-allocation tags to a specified stack or layer in AWS OpsWorks
-- Stacks. For more information about how tagging works, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/tagging.html Tags>
-- in the AWS OpsWorks User Guide.
module Network.AWS.OpsWorks.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceArn,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The stack or layer\'s Amazon Resource Number (ARN).
    resourceArn :: Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to a stack
    -- or layer.
    --
    -- -   The key cannot be empty.
    --
    -- -   The key can be a maximum of 127 characters, and can contain only
    --     Unicode letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/@
    --
    -- -   The value can be a maximum 255 characters, and contain only Unicode
    --     letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/@
    --
    -- -   Leading and trailing white spaces are trimmed from both the key and
    --     value.
    --
    -- -   A maximum of 40 tags is allowed for any resource.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'tagResource_resourceArn' - The stack or layer\'s Amazon Resource Number (ARN).
--
-- 'tags', 'tagResource_tags' - A map that contains tag keys and tag values that are attached to a stack
-- or layer.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 40 tags is allowed for any resource.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The stack or layer\'s Amazon Resource Number (ARN).
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | A map that contains tag keys and tag values that are attached to a stack
-- or layer.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 40 tags is allowed for any resource.
tagResource_tags :: Lens.Lens' TagResource (Prelude.HashMap Prelude.Text Prelude.Text)
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Prelude.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.TagResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Prelude..= resourceArn),
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath TagResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse
