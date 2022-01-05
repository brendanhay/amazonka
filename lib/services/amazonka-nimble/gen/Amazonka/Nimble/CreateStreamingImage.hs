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
-- Module      : Amazonka.Nimble.CreateStreamingImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming image resource in a studio.
module Amazonka.Nimble.CreateStreamingImage
  ( -- * Creating a Request
    CreateStreamingImage (..),
    newCreateStreamingImage,

    -- * Request Lenses
    createStreamingImage_clientToken,
    createStreamingImage_description,
    createStreamingImage_tags,
    createStreamingImage_studioId,
    createStreamingImage_name,
    createStreamingImage_ec2ImageId,

    -- * Destructuring the Response
    CreateStreamingImageResponse (..),
    newCreateStreamingImageResponse,

    -- * Response Lenses
    createStreamingImageResponse_streamingImage,
    createStreamingImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A collection of streaming images.
--
-- /See:/ 'newCreateStreamingImage' smart constructor.
data CreateStreamingImage = CreateStreamingImage'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the streaming image.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | A friendly name for a streaming image resource.
    name :: Prelude.Text,
    -- | The ID of an EC2 machine image with which to create this streaming
    -- image.
    ec2ImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createStreamingImage_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'description', 'createStreamingImage_description' - A human-readable description of the streaming image.
--
-- 'tags', 'createStreamingImage_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'studioId', 'createStreamingImage_studioId' - The studio ID.
--
-- 'name', 'createStreamingImage_name' - A friendly name for a streaming image resource.
--
-- 'ec2ImageId', 'createStreamingImage_ec2ImageId' - The ID of an EC2 machine image with which to create this streaming
-- image.
newCreateStreamingImage ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ec2ImageId'
  Prelude.Text ->
  CreateStreamingImage
newCreateStreamingImage
  pStudioId_
  pName_
  pEc2ImageId_ =
    CreateStreamingImage'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        studioId = pStudioId_,
        name = pName_,
        ec2ImageId = pEc2ImageId_
      }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
createStreamingImage_clientToken :: Lens.Lens' CreateStreamingImage (Prelude.Maybe Prelude.Text)
createStreamingImage_clientToken = Lens.lens (\CreateStreamingImage' {clientToken} -> clientToken) (\s@CreateStreamingImage' {} a -> s {clientToken = a} :: CreateStreamingImage)

-- | A human-readable description of the streaming image.
createStreamingImage_description :: Lens.Lens' CreateStreamingImage (Prelude.Maybe Prelude.Text)
createStreamingImage_description = Lens.lens (\CreateStreamingImage' {description} -> description) (\s@CreateStreamingImage' {} a -> s {description = a} :: CreateStreamingImage)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStreamingImage_tags :: Lens.Lens' CreateStreamingImage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamingImage_tags = Lens.lens (\CreateStreamingImage' {tags} -> tags) (\s@CreateStreamingImage' {} a -> s {tags = a} :: CreateStreamingImage) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
createStreamingImage_studioId :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_studioId = Lens.lens (\CreateStreamingImage' {studioId} -> studioId) (\s@CreateStreamingImage' {} a -> s {studioId = a} :: CreateStreamingImage)

-- | A friendly name for a streaming image resource.
createStreamingImage_name :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_name = Lens.lens (\CreateStreamingImage' {name} -> name) (\s@CreateStreamingImage' {} a -> s {name = a} :: CreateStreamingImage)

-- | The ID of an EC2 machine image with which to create this streaming
-- image.
createStreamingImage_ec2ImageId :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_ec2ImageId = Lens.lens (\CreateStreamingImage' {ec2ImageId} -> ec2ImageId) (\s@CreateStreamingImage' {} a -> s {ec2ImageId = a} :: CreateStreamingImage)

instance Core.AWSRequest CreateStreamingImage where
  type
    AWSResponse CreateStreamingImage =
      CreateStreamingImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingImageResponse'
            Prelude.<$> (x Core..?> "streamingImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingImage where
  hashWithSalt _salt CreateStreamingImage' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ec2ImageId

instance Prelude.NFData CreateStreamingImage where
  rnf CreateStreamingImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ec2ImageId

instance Core.ToHeaders CreateStreamingImage where
  toHeaders CreateStreamingImage' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateStreamingImage where
  toJSON CreateStreamingImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("ec2ImageId" Core..= ec2ImageId)
          ]
      )

instance Core.ToPath CreateStreamingImage where
  toPath CreateStreamingImage' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-images"
      ]

instance Core.ToQuery CreateStreamingImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingImageResponse' smart constructor.
data CreateStreamingImageResponse = CreateStreamingImageResponse'
  { streamingImage :: Prelude.Maybe StreamingImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImage', 'createStreamingImageResponse_streamingImage' -
--
-- 'httpStatus', 'createStreamingImageResponse_httpStatus' - The response's http status code.
newCreateStreamingImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamingImageResponse
newCreateStreamingImageResponse pHttpStatus_ =
  CreateStreamingImageResponse'
    { streamingImage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
createStreamingImageResponse_streamingImage :: Lens.Lens' CreateStreamingImageResponse (Prelude.Maybe StreamingImage)
createStreamingImageResponse_streamingImage = Lens.lens (\CreateStreamingImageResponse' {streamingImage} -> streamingImage) (\s@CreateStreamingImageResponse' {} a -> s {streamingImage = a} :: CreateStreamingImageResponse)

-- | The response's http status code.
createStreamingImageResponse_httpStatus :: Lens.Lens' CreateStreamingImageResponse Prelude.Int
createStreamingImageResponse_httpStatus = Lens.lens (\CreateStreamingImageResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingImageResponse' {} a -> s {httpStatus = a} :: CreateStreamingImageResponse)

instance Prelude.NFData CreateStreamingImageResponse where
  rnf CreateStreamingImageResponse' {..} =
    Prelude.rnf streamingImage
      `Prelude.seq` Prelude.rnf httpStatus
