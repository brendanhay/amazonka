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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createStreamingImage_ec2ImageId,
    createStreamingImage_name,
    createStreamingImage_studioId,

    -- * Destructuring the Response
    CreateStreamingImageResponse (..),
    newCreateStreamingImageResponse,

    -- * Response Lenses
    createStreamingImageResponse_streamingImage,
    createStreamingImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStreamingImage' smart constructor.
data CreateStreamingImage = CreateStreamingImage'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the streaming image.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of an EC2 machine image with which to create this streaming
    -- image.
    ec2ImageId :: Prelude.Text,
    -- | A friendly name for a streaming image resource.
    name :: Data.Sensitive Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createStreamingImage_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'description', 'createStreamingImage_description' - A human-readable description of the streaming image.
--
-- 'tags', 'createStreamingImage_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'ec2ImageId', 'createStreamingImage_ec2ImageId' - The ID of an EC2 machine image with which to create this streaming
-- image.
--
-- 'name', 'createStreamingImage_name' - A friendly name for a streaming image resource.
--
-- 'studioId', 'createStreamingImage_studioId' - The studio ID.
newCreateStreamingImage ::
  -- | 'ec2ImageId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  CreateStreamingImage
newCreateStreamingImage
  pEc2ImageId_
  pName_
  pStudioId_ =
    CreateStreamingImage'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        ec2ImageId = pEc2ImageId_,
        name = Data._Sensitive Lens.# pName_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
createStreamingImage_clientToken :: Lens.Lens' CreateStreamingImage (Prelude.Maybe Prelude.Text)
createStreamingImage_clientToken = Lens.lens (\CreateStreamingImage' {clientToken} -> clientToken) (\s@CreateStreamingImage' {} a -> s {clientToken = a} :: CreateStreamingImage)

-- | A human-readable description of the streaming image.
createStreamingImage_description :: Lens.Lens' CreateStreamingImage (Prelude.Maybe Prelude.Text)
createStreamingImage_description = Lens.lens (\CreateStreamingImage' {description} -> description) (\s@CreateStreamingImage' {} a -> s {description = a} :: CreateStreamingImage) Prelude.. Lens.mapping Data._Sensitive

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStreamingImage_tags :: Lens.Lens' CreateStreamingImage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamingImage_tags = Lens.lens (\CreateStreamingImage' {tags} -> tags) (\s@CreateStreamingImage' {} a -> s {tags = a} :: CreateStreamingImage) Prelude.. Lens.mapping Lens.coerced

-- | The ID of an EC2 machine image with which to create this streaming
-- image.
createStreamingImage_ec2ImageId :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_ec2ImageId = Lens.lens (\CreateStreamingImage' {ec2ImageId} -> ec2ImageId) (\s@CreateStreamingImage' {} a -> s {ec2ImageId = a} :: CreateStreamingImage)

-- | A friendly name for a streaming image resource.
createStreamingImage_name :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_name = Lens.lens (\CreateStreamingImage' {name} -> name) (\s@CreateStreamingImage' {} a -> s {name = a} :: CreateStreamingImage) Prelude.. Data._Sensitive

-- | The studio ID.
createStreamingImage_studioId :: Lens.Lens' CreateStreamingImage Prelude.Text
createStreamingImage_studioId = Lens.lens (\CreateStreamingImage' {studioId} -> studioId) (\s@CreateStreamingImage' {} a -> s {studioId = a} :: CreateStreamingImage)

instance Core.AWSRequest CreateStreamingImage where
  type
    AWSResponse CreateStreamingImage =
      CreateStreamingImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingImageResponse'
            Prelude.<$> (x Data..?> "streamingImage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingImage where
  hashWithSalt _salt CreateStreamingImage' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ec2ImageId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData CreateStreamingImage where
  rnf CreateStreamingImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ec2ImageId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders CreateStreamingImage where
  toHeaders CreateStreamingImage' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateStreamingImage where
  toJSON CreateStreamingImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ec2ImageId" Data..= ec2ImageId),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateStreamingImage where
  toPath CreateStreamingImage' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-images"
      ]

instance Data.ToQuery CreateStreamingImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingImageResponse' smart constructor.
data CreateStreamingImageResponse = CreateStreamingImageResponse'
  { -- | The streaming image.
    streamingImage :: Prelude.Maybe StreamingImage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingImage', 'createStreamingImageResponse_streamingImage' - The streaming image.
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

-- | The streaming image.
createStreamingImageResponse_streamingImage :: Lens.Lens' CreateStreamingImageResponse (Prelude.Maybe StreamingImage)
createStreamingImageResponse_streamingImage = Lens.lens (\CreateStreamingImageResponse' {streamingImage} -> streamingImage) (\s@CreateStreamingImageResponse' {} a -> s {streamingImage = a} :: CreateStreamingImageResponse)

-- | The response's http status code.
createStreamingImageResponse_httpStatus :: Lens.Lens' CreateStreamingImageResponse Prelude.Int
createStreamingImageResponse_httpStatus = Lens.lens (\CreateStreamingImageResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingImageResponse' {} a -> s {httpStatus = a} :: CreateStreamingImageResponse)

instance Prelude.NFData CreateStreamingImageResponse where
  rnf CreateStreamingImageResponse' {..} =
    Prelude.rnf streamingImage
      `Prelude.seq` Prelude.rnf httpStatus
