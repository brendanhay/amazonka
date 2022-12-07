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
-- Module      : Amazonka.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Amazonka.SageMaker.DescribeImage
  ( -- * Creating a Request
    DescribeImage (..),
    newDescribeImage,

    -- * Request Lenses
    describeImage_imageName,

    -- * Destructuring the Response
    DescribeImageResponse (..),
    newDescribeImageResponse,

    -- * Response Lenses
    describeImageResponse_roleArn,
    describeImageResponse_imageStatus,
    describeImageResponse_displayName,
    describeImageResponse_imageArn,
    describeImageResponse_description,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_creationTime,
    describeImageResponse_failureReason,
    describeImageResponse_imageName,
    describeImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageResponse'
            Prelude.<$> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "ImageStatus")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "ImageArn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "ImageName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImage where
  hashWithSalt _salt DescribeImage' {..} =
    _salt `Prelude.hashWithSalt` imageName

instance Prelude.NFData DescribeImage where
  rnf DescribeImage' {..} = Prelude.rnf imageName

instance Data.ToHeaders DescribeImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeImage" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImage where
  toJSON DescribeImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ImageName" Data..= imageName)]
      )

instance Data.ToPath DescribeImage where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { -- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the image.
    imageStatus :: Prelude.Maybe ImageStatus,
    -- | The name of the image as displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the image was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | When the image was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | When a create, update, or delete operation fails, the reason for the
    -- failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the image.
    imageName :: Prelude.Maybe Prelude.Text,
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
-- 'roleArn', 'describeImageResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'imageStatus', 'describeImageResponse_imageStatus' - The status of the image.
--
-- 'displayName', 'describeImageResponse_displayName' - The name of the image as displayed.
--
-- 'imageArn', 'describeImageResponse_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'description', 'describeImageResponse_description' - The description of the image.
--
-- 'lastModifiedTime', 'describeImageResponse_lastModifiedTime' - When the image was last modified.
--
-- 'creationTime', 'describeImageResponse_creationTime' - When the image was created.
--
-- 'failureReason', 'describeImageResponse_failureReason' - When a create, update, or delete operation fails, the reason for the
-- failure.
--
-- 'imageName', 'describeImageResponse_imageName' - The name of the image.
--
-- 'httpStatus', 'describeImageResponse_httpStatus' - The response's http status code.
newDescribeImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageResponse
newDescribeImageResponse pHttpStatus_ =
  DescribeImageResponse'
    { roleArn = Prelude.Nothing,
      imageStatus = Prelude.Nothing,
      displayName = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      imageName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
describeImageResponse_roleArn :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_roleArn = Lens.lens (\DescribeImageResponse' {roleArn} -> roleArn) (\s@DescribeImageResponse' {} a -> s {roleArn = a} :: DescribeImageResponse)

-- | The status of the image.
describeImageResponse_imageStatus :: Lens.Lens' DescribeImageResponse (Prelude.Maybe ImageStatus)
describeImageResponse_imageStatus = Lens.lens (\DescribeImageResponse' {imageStatus} -> imageStatus) (\s@DescribeImageResponse' {} a -> s {imageStatus = a} :: DescribeImageResponse)

-- | The name of the image as displayed.
describeImageResponse_displayName :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_displayName = Lens.lens (\DescribeImageResponse' {displayName} -> displayName) (\s@DescribeImageResponse' {} a -> s {displayName = a} :: DescribeImageResponse)

-- | The Amazon Resource Name (ARN) of the image.
describeImageResponse_imageArn :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_imageArn = Lens.lens (\DescribeImageResponse' {imageArn} -> imageArn) (\s@DescribeImageResponse' {} a -> s {imageArn = a} :: DescribeImageResponse)

-- | The description of the image.
describeImageResponse_description :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_description = Lens.lens (\DescribeImageResponse' {description} -> description) (\s@DescribeImageResponse' {} a -> s {description = a} :: DescribeImageResponse)

-- | When the image was last modified.
describeImageResponse_lastModifiedTime :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.UTCTime)
describeImageResponse_lastModifiedTime = Lens.lens (\DescribeImageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeImageResponse' {} a -> s {lastModifiedTime = a} :: DescribeImageResponse) Prelude.. Lens.mapping Data._Time

-- | When the image was created.
describeImageResponse_creationTime :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.UTCTime)
describeImageResponse_creationTime = Lens.lens (\DescribeImageResponse' {creationTime} -> creationTime) (\s@DescribeImageResponse' {} a -> s {creationTime = a} :: DescribeImageResponse) Prelude.. Lens.mapping Data._Time

-- | When a create, update, or delete operation fails, the reason for the
-- failure.
describeImageResponse_failureReason :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_failureReason = Lens.lens (\DescribeImageResponse' {failureReason} -> failureReason) (\s@DescribeImageResponse' {} a -> s {failureReason = a} :: DescribeImageResponse)

-- | The name of the image.
describeImageResponse_imageName :: Lens.Lens' DescribeImageResponse (Prelude.Maybe Prelude.Text)
describeImageResponse_imageName = Lens.lens (\DescribeImageResponse' {imageName} -> imageName) (\s@DescribeImageResponse' {} a -> s {imageName = a} :: DescribeImageResponse)

-- | The response's http status code.
describeImageResponse_httpStatus :: Lens.Lens' DescribeImageResponse Prelude.Int
describeImageResponse_httpStatus = Lens.lens (\DescribeImageResponse' {httpStatus} -> httpStatus) (\s@DescribeImageResponse' {} a -> s {httpStatus = a} :: DescribeImageResponse)

instance Prelude.NFData DescribeImageResponse where
  rnf DescribeImageResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf imageStatus
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf httpStatus
