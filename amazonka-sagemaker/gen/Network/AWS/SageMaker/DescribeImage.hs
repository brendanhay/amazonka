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
    describeImageResponse_roleArn,
    describeImageResponse_imageName,
    describeImageResponse_failureReason,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_description,
    describeImageResponse_imageStatus,
    describeImageResponse_displayName,
    describeImageResponse_imageArn,
    describeImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeImage' smart constructor.
data DescribeImage = DescribeImage'
  { -- | The name of the image to describe.
    imageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeImage
newDescribeImage pImageName_ =
  DescribeImage' {imageName = pImageName_}

-- | The name of the image to describe.
describeImage_imageName :: Lens.Lens' DescribeImage Core.Text
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
            Core.<$> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "ImageName")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "ImageStatus")
            Core.<*> (x Core..?> "DisplayName")
            Core.<*> (x Core..?> "ImageArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImage

instance Core.NFData DescribeImage

instance Core.ToHeaders DescribeImage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeImage" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImage where
  toJSON DescribeImage' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ImageName" Core..= imageName)]
      )

instance Core.ToPath DescribeImage where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { -- | When the image was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Core.Maybe Core.Text,
    -- | The name of the image.
    imageName :: Core.Maybe Core.Text,
    -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Core.Maybe Core.Text,
    -- | When the image was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The description of the image.
    description :: Core.Maybe Core.Text,
    -- | The status of the image.
    imageStatus :: Core.Maybe ImageStatus,
    -- | The name of the image as displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'roleArn', 'describeImageResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'imageName', 'describeImageResponse_imageName' - The name of the image.
--
-- 'failureReason', 'describeImageResponse_failureReason' - When a create, update, or delete operation fails, the reason for the
-- failure.
--
-- 'lastModifiedTime', 'describeImageResponse_lastModifiedTime' - When the image was last modified.
--
-- 'description', 'describeImageResponse_description' - The description of the image.
--
-- 'imageStatus', 'describeImageResponse_imageStatus' - The status of the image.
--
-- 'displayName', 'describeImageResponse_displayName' - The name of the image as displayed.
--
-- 'imageArn', 'describeImageResponse_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'httpStatus', 'describeImageResponse_httpStatus' - The response's http status code.
newDescribeImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImageResponse
newDescribeImageResponse pHttpStatus_ =
  DescribeImageResponse'
    { creationTime = Core.Nothing,
      roleArn = Core.Nothing,
      imageName = Core.Nothing,
      failureReason = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      description = Core.Nothing,
      imageStatus = Core.Nothing,
      displayName = Core.Nothing,
      imageArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the image was created.
describeImageResponse_creationTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.UTCTime)
describeImageResponse_creationTime = Lens.lens (\DescribeImageResponse' {creationTime} -> creationTime) (\s@DescribeImageResponse' {} a -> s {creationTime = a} :: DescribeImageResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
describeImageResponse_roleArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_roleArn = Lens.lens (\DescribeImageResponse' {roleArn} -> roleArn) (\s@DescribeImageResponse' {} a -> s {roleArn = a} :: DescribeImageResponse)

-- | The name of the image.
describeImageResponse_imageName :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_imageName = Lens.lens (\DescribeImageResponse' {imageName} -> imageName) (\s@DescribeImageResponse' {} a -> s {imageName = a} :: DescribeImageResponse)

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
describeImageResponse_failureReason :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_failureReason = Lens.lens (\DescribeImageResponse' {failureReason} -> failureReason) (\s@DescribeImageResponse' {} a -> s {failureReason = a} :: DescribeImageResponse)

-- | When the image was last modified.
describeImageResponse_lastModifiedTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.UTCTime)
describeImageResponse_lastModifiedTime = Lens.lens (\DescribeImageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeImageResponse' {} a -> s {lastModifiedTime = a} :: DescribeImageResponse) Core.. Lens.mapping Core._Time

-- | The description of the image.
describeImageResponse_description :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_description = Lens.lens (\DescribeImageResponse' {description} -> description) (\s@DescribeImageResponse' {} a -> s {description = a} :: DescribeImageResponse)

-- | The status of the image.
describeImageResponse_imageStatus :: Lens.Lens' DescribeImageResponse (Core.Maybe ImageStatus)
describeImageResponse_imageStatus = Lens.lens (\DescribeImageResponse' {imageStatus} -> imageStatus) (\s@DescribeImageResponse' {} a -> s {imageStatus = a} :: DescribeImageResponse)

-- | The name of the image as displayed.
describeImageResponse_displayName :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_displayName = Lens.lens (\DescribeImageResponse' {displayName} -> displayName) (\s@DescribeImageResponse' {} a -> s {displayName = a} :: DescribeImageResponse)

-- | The Amazon Resource Name (ARN) of the image.
describeImageResponse_imageArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.Text)
describeImageResponse_imageArn = Lens.lens (\DescribeImageResponse' {imageArn} -> imageArn) (\s@DescribeImageResponse' {} a -> s {imageArn = a} :: DescribeImageResponse)

-- | The response's http status code.
describeImageResponse_httpStatus :: Lens.Lens' DescribeImageResponse Core.Int
describeImageResponse_httpStatus = Lens.lens (\DescribeImageResponse' {httpStatus} -> httpStatus) (\s@DescribeImageResponse' {} a -> s {httpStatus = a} :: DescribeImageResponse)

instance Core.NFData DescribeImageResponse
