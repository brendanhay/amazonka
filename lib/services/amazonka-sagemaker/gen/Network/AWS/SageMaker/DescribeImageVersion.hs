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
-- Module      : Network.AWS.SageMaker.DescribeImageVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of a SageMaker image.
module Network.AWS.SageMaker.DescribeImageVersion
  ( -- * Creating a Request
    DescribeImageVersion (..),
    newDescribeImageVersion,

    -- * Request Lenses
    describeImageVersion_version,
    describeImageVersion_imageName,

    -- * Destructuring the Response
    DescribeImageVersionResponse (..),
    newDescribeImageVersionResponse,

    -- * Response Lenses
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_version,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeImageVersion' smart constructor.
data DescribeImageVersion = DescribeImageVersion'
  { -- | The version of the image. If not specified, the latest version is
    -- described.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the image.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'describeImageVersion_version' - The version of the image. If not specified, the latest version is
-- described.
--
-- 'imageName', 'describeImageVersion_imageName' - The name of the image.
newDescribeImageVersion ::
  -- | 'imageName'
  Prelude.Text ->
  DescribeImageVersion
newDescribeImageVersion pImageName_ =
  DescribeImageVersion'
    { version = Prelude.Nothing,
      imageName = pImageName_
    }

-- | The version of the image. If not specified, the latest version is
-- described.
describeImageVersion_version :: Lens.Lens' DescribeImageVersion (Prelude.Maybe Prelude.Natural)
describeImageVersion_version = Lens.lens (\DescribeImageVersion' {version} -> version) (\s@DescribeImageVersion' {} a -> s {version = a} :: DescribeImageVersion)

-- | The name of the image.
describeImageVersion_imageName :: Lens.Lens' DescribeImageVersion Prelude.Text
describeImageVersion_imageName = Lens.lens (\DescribeImageVersion' {imageName} -> imageName) (\s@DescribeImageVersion' {} a -> s {imageName = a} :: DescribeImageVersion)

instance Core.AWSRequest DescribeImageVersion where
  type
    AWSResponse DescribeImageVersion =
      DescribeImageVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageVersionResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "ImageVersionStatus")
            Prelude.<*> (x Core..?> "ContainerImage")
            Prelude.<*> (x Core..?> "ImageVersionArn")
            Prelude.<*> (x Core..?> "BaseImage")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ImageArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageVersion

instance Prelude.NFData DescribeImageVersion

instance Core.ToHeaders DescribeImageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeImageVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImageVersion where
  toJSON DescribeImageVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Version" Core..=) Prelude.<$> version,
            Prelude.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.ToPath DescribeImageVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageVersionResponse' smart constructor.
data DescribeImageVersionResponse = DescribeImageVersionResponse'
  { -- | When the version was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the version.
    imageVersionStatus :: Prelude.Maybe ImageVersionStatus,
    -- | The registry path of the container image that contains this image
    -- version.
    containerImage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    imageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The registry path of the container image on which this image version is
    -- based.
    baseImage :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Natural,
    -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | When the version was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeImageVersionResponse_creationTime' - When the version was created.
--
-- 'imageVersionStatus', 'describeImageVersionResponse_imageVersionStatus' - The status of the version.
--
-- 'containerImage', 'describeImageVersionResponse_containerImage' - The registry path of the container image that contains this image
-- version.
--
-- 'imageVersionArn', 'describeImageVersionResponse_imageVersionArn' - The ARN of the version.
--
-- 'baseImage', 'describeImageVersionResponse_baseImage' - The registry path of the container image on which this image version is
-- based.
--
-- 'version', 'describeImageVersionResponse_version' - The version number.
--
-- 'failureReason', 'describeImageVersionResponse_failureReason' - When a create or delete operation fails, the reason for the failure.
--
-- 'lastModifiedTime', 'describeImageVersionResponse_lastModifiedTime' - When the version was last modified.
--
-- 'imageArn', 'describeImageVersionResponse_imageArn' - The Amazon Resource Name (ARN) of the image the version is based on.
--
-- 'httpStatus', 'describeImageVersionResponse_httpStatus' - The response's http status code.
newDescribeImageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageVersionResponse
newDescribeImageVersionResponse pHttpStatus_ =
  DescribeImageVersionResponse'
    { creationTime =
        Prelude.Nothing,
      imageVersionStatus = Prelude.Nothing,
      containerImage = Prelude.Nothing,
      imageVersionArn = Prelude.Nothing,
      baseImage = Prelude.Nothing,
      version = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the version was created.
describeImageVersionResponse_creationTime :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeImageVersionResponse_creationTime = Lens.lens (\DescribeImageVersionResponse' {creationTime} -> creationTime) (\s@DescribeImageVersionResponse' {} a -> s {creationTime = a} :: DescribeImageVersionResponse) Prelude.. Lens.mapping Core._Time

-- | The status of the version.
describeImageVersionResponse_imageVersionStatus :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe ImageVersionStatus)
describeImageVersionResponse_imageVersionStatus = Lens.lens (\DescribeImageVersionResponse' {imageVersionStatus} -> imageVersionStatus) (\s@DescribeImageVersionResponse' {} a -> s {imageVersionStatus = a} :: DescribeImageVersionResponse)

-- | The registry path of the container image that contains this image
-- version.
describeImageVersionResponse_containerImage :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_containerImage = Lens.lens (\DescribeImageVersionResponse' {containerImage} -> containerImage) (\s@DescribeImageVersionResponse' {} a -> s {containerImage = a} :: DescribeImageVersionResponse)

-- | The ARN of the version.
describeImageVersionResponse_imageVersionArn :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_imageVersionArn = Lens.lens (\DescribeImageVersionResponse' {imageVersionArn} -> imageVersionArn) (\s@DescribeImageVersionResponse' {} a -> s {imageVersionArn = a} :: DescribeImageVersionResponse)

-- | The registry path of the container image on which this image version is
-- based.
describeImageVersionResponse_baseImage :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_baseImage = Lens.lens (\DescribeImageVersionResponse' {baseImage} -> baseImage) (\s@DescribeImageVersionResponse' {} a -> s {baseImage = a} :: DescribeImageVersionResponse)

-- | The version number.
describeImageVersionResponse_version :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Natural)
describeImageVersionResponse_version = Lens.lens (\DescribeImageVersionResponse' {version} -> version) (\s@DescribeImageVersionResponse' {} a -> s {version = a} :: DescribeImageVersionResponse)

-- | When a create or delete operation fails, the reason for the failure.
describeImageVersionResponse_failureReason :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_failureReason = Lens.lens (\DescribeImageVersionResponse' {failureReason} -> failureReason) (\s@DescribeImageVersionResponse' {} a -> s {failureReason = a} :: DescribeImageVersionResponse)

-- | When the version was last modified.
describeImageVersionResponse_lastModifiedTime :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeImageVersionResponse_lastModifiedTime = Lens.lens (\DescribeImageVersionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeImageVersionResponse' {} a -> s {lastModifiedTime = a} :: DescribeImageVersionResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the image the version is based on.
describeImageVersionResponse_imageArn :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_imageArn = Lens.lens (\DescribeImageVersionResponse' {imageArn} -> imageArn) (\s@DescribeImageVersionResponse' {} a -> s {imageArn = a} :: DescribeImageVersionResponse)

-- | The response's http status code.
describeImageVersionResponse_httpStatus :: Lens.Lens' DescribeImageVersionResponse Prelude.Int
describeImageVersionResponse_httpStatus = Lens.lens (\DescribeImageVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeImageVersionResponse' {} a -> s {httpStatus = a} :: DescribeImageVersionResponse)

instance Prelude.NFData DescribeImageVersionResponse
