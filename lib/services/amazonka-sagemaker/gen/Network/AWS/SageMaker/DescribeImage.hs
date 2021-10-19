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
-- Module      : Network.AWS.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Network.AWS.SageMaker.DescribeImage
  ( -- * Creating a Request
    DescribeImage (..),
    newDescribeImage,

    -- * Request Lenses
    describeImage_imageName,

    -- * Destructuring the Response
    DescribeImageResponse (..),
    newDescribeImageResponse,

    -- * Response Lenses
    describeImageResponse_creationTime,
    describeImageResponse_failureReason,
    describeImageResponse_imageStatus,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_imageArn,
    describeImageResponse_displayName,
    describeImageResponse_imageName,
    describeImageResponse_description,
    describeImageResponse_roleArn,
    describeImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeImage' smart constructor.
data DescribeImage = DescribeImage'
  { -- | The name of the image to describe.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageName', 'describeImage_imageName' - The name of the image to describe.
newDescribeImage ::
  -- | 'imageName'
  Prelude.Text ->
  DescribeImage
newDescribeImage pImageName_ =
  DescribeImage' {imageName = pImageName_}

-- | The name of the image to describe.
describeImage_imageName :: Lens.Lens' DescribeImage Prelude.Text
describeImage_imageName = Lens.lens (\DescribeImage' {imageName} -> imageName) (\s@DescribeImage' {} a -> s {imageName = a} :: DescribeImage)

instance Core.AWSRequest DescribeImage where
  type
    AWSResponse DescribeImage =
      DescribeImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "ImageStatus")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ImageArn")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "ImageName")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImage

instance Prelude.NFData DescribeImage

instance Core.ToHeaders DescribeImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeImage" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImage where
  toJSON DescribeImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImageName" Core..= imageName)]
      )

instance Core.ToPath DescribeImage where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { -- | When the image was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the image.
    imageStatus :: Prelude.Maybe ImageStatus,
    -- | When the image was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the image as displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the image.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeImageResponse_creationTime' - When the image was created.
--
-- 'failureReason', 'describeImageResponse_failureReason' - When a create, update, or delete operation fails, the reason for the
-- failure.
--
-- 'imageStatus', 'describeImageResponse_imageStatus' - The status of the image.
--
-- 'lastModifiedTime', 'describeImageResponse_lastModifiedTime' - When the image was last modified.
--
-- 'imageArn', 'describeImageResponse_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'displayName', 'describeImageResponse_displayName' - The name of the image as displayed.
--
-- 'imageName', 'describeImageResponse_imageName' - The name of the image.
--
-- 'description', 'describeImageResponse_description' - The description of the image.
--
-- 'roleArn', 'describeImageResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'httpStatus', 'describeImageResponse_httpStatus' - The response's http status code.
newDescribeImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageResponse
newDescribeImageResponse pHttpStatus_ =
  DescribeImageResponse'
    { creationTime =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      imageStatus = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      imageName = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the image was created.
describeImageResponse_creationTime :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.UTCTime)
describeImageResponse_creationTime = Lens.lens (\DescribeImageResponse' {creationTime} -> creationTime) (\s@DescribeImageResponse' {} a -> s {creationTime = a} :: DescribeImageResponse) Prelude.. Lens.mapping Core._Time

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
describeImageResponse_failureReason :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_failureReason = Lens.lens (\DescribeImageResponse' {failureReason} -> failureReason) (\s@DescribeImageResponse' {} a -> s {failureReason = a} :: DescribeImageResponse)

-- | The status of the image.
describeImageResponse_imageStatus :: Lens.Lens' DescribeImageResponse (Prelude.Maybe ImageStatus)
describeImageResponse_imageStatus = Lens.lens (\DescribeImageResponse' {imageStatus} -> imageStatus) (\s@DescribeImageResponse' {} a -> s {imageStatus = a} :: DescribeImageResponse)

-- | When the image was last modified.
describeImageResponse_lastModifiedTime :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.UTCTime)
describeImageResponse_lastModifiedTime = Lens.lens (\DescribeImageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeImageResponse' {} a -> s {lastModifiedTime = a} :: DescribeImageResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the image.
describeImageResponse_imageArn :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_imageArn = Lens.lens (\DescribeImageResponse' {imageArn} -> imageArn) (\s@DescribeImageResponse' {} a -> s {imageArn = a} :: DescribeImageResponse)

-- | The name of the image as displayed.
describeImageResponse_displayName :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_displayName = Lens.lens (\DescribeImageResponse' {displayName} -> displayName) (\s@DescribeImageResponse' {} a -> s {displayName = a} :: DescribeImageResponse)

-- | The name of the image.
describeImageResponse_imageName :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_imageName = Lens.lens (\DescribeImageResponse' {imageName} -> imageName) (\s@DescribeImageResponse' {} a -> s {imageName = a} :: DescribeImageResponse)

-- | The description of the image.
describeImageResponse_description :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_description = Lens.lens (\DescribeImageResponse' {description} -> description) (\s@DescribeImageResponse' {} a -> s {description = a} :: DescribeImageResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
describeImageResponse_roleArn :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_roleArn = Lens.lens (\DescribeImageResponse' {roleArn} -> roleArn) (\s@DescribeImageResponse' {} a -> s {roleArn = a} :: DescribeImageResponse)

-- | The response's http status code.
describeImageResponse_httpStatus :: Lens.Lens' DescribeImageResponse Prelude.Int
describeImageResponse_httpStatus = Lens.lens (\DescribeImageResponse' {httpStatus} -> httpStatus) (\s@DescribeImageResponse' {} a -> s {httpStatus = a} :: DescribeImageResponse)

instance Prelude.NFData DescribeImageResponse
