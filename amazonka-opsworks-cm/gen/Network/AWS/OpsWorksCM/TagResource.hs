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
-- Module      : Network.AWS.OpsWorksCM.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies tags to an AWS OpsWorks for Chef Automate or AWS OpsWorks for
-- Puppet Enterprise server, or to server backups.
module Network.AWS.OpsWorksCM.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceArn,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Number (ARN) of a resource to which you want to
    -- apply tags. For example,
    -- @arn:aws:opsworks-cm:us-west-2:123456789012:server\/test-owcm-server\/EXAMPLE-66b0-4196-8274-d1a2bEXAMPLE@.
    resourceArn :: Prelude.Text,
    -- | A map that contains tag keys and tag values to attach to AWS OpsWorks-CM
    -- servers or backups.
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
    -- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
    --     server or backup.
    tags :: [Tag]
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
-- 'resourceArn', 'tagResource_resourceArn' - The Amazon Resource Number (ARN) of a resource to which you want to
-- apply tags. For example,
-- @arn:aws:opsworks-cm:us-west-2:123456789012:server\/test-owcm-server\/EXAMPLE-66b0-4196-8274-d1a2bEXAMPLE@.
--
-- 'tags', 'tagResource_tags' - A map that contains tag keys and tag values to attach to AWS OpsWorks-CM
-- servers or backups.
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
-- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
--     server or backup.
newTagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  TagResource
newTagResource pResourceArn_ =
  TagResource'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Number (ARN) of a resource to which you want to
-- apply tags. For example,
-- @arn:aws:opsworks-cm:us-west-2:123456789012:server\/test-owcm-server\/EXAMPLE-66b0-4196-8274-d1a2bEXAMPLE@.
tagResource_resourceArn :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceArn = Lens.lens (\TagResource' {resourceArn} -> resourceArn) (\s@TagResource' {} a -> s {resourceArn = a} :: TagResource)

-- | A map that contains tag keys and tag values to attach to AWS OpsWorks-CM
-- servers or backups.
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
-- -   A maximum of 50 user-applied tags is allowed for any AWS OpsWorks-CM
--     server or backup.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Prelude.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorksCM_V2016_11_01.TagResource" ::
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
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
