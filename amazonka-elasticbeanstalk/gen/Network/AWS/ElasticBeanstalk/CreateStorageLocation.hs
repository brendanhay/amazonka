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
-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bucket in Amazon S3 to store application versions, logs, and
-- other files used by Elastic Beanstalk environments. The Elastic
-- Beanstalk console and EB CLI call this API the first time you create an
-- environment in a region. If the storage location already exists,
-- @CreateStorageLocation@ still returns the bucket name but does not
-- create a new bucket.
module Network.AWS.ElasticBeanstalk.CreateStorageLocation
  ( -- * Creating a Request
    CreateStorageLocation (..),
    newCreateStorageLocation,

    -- * Destructuring the Response
    CreateStorageLocationResponse (..),
    newCreateStorageLocationResponse,

    -- * Response Lenses
    createStorageLocationResponse_s3Bucket,
    createStorageLocationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStorageLocation' smart constructor.
data CreateStorageLocation = CreateStorageLocation'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStorageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateStorageLocation ::
  CreateStorageLocation
newCreateStorageLocation = CreateStorageLocation'

instance Core.AWSRequest CreateStorageLocation where
  type
    AWSResponse CreateStorageLocation =
      CreateStorageLocationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateStorageLocationResult"
      ( \s h x ->
          CreateStorageLocationResponse'
            Core.<$> (x Core..@? "S3Bucket")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStorageLocation

instance Core.NFData CreateStorageLocation

instance Core.ToHeaders CreateStorageLocation where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateStorageLocation where
  toPath = Core.const "/"

instance Core.ToQuery CreateStorageLocation where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("CreateStorageLocation" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | Results of a CreateStorageLocationResult call.
--
-- /See:/ 'newCreateStorageLocationResponse' smart constructor.
data CreateStorageLocationResponse = CreateStorageLocationResponse'
  { -- | The name of the Amazon S3 bucket created.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStorageLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'createStorageLocationResponse_s3Bucket' - The name of the Amazon S3 bucket created.
--
-- 'httpStatus', 'createStorageLocationResponse_httpStatus' - The response's http status code.
newCreateStorageLocationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateStorageLocationResponse
newCreateStorageLocationResponse pHttpStatus_ =
  CreateStorageLocationResponse'
    { s3Bucket =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Amazon S3 bucket created.
createStorageLocationResponse_s3Bucket :: Lens.Lens' CreateStorageLocationResponse (Core.Maybe Core.Text)
createStorageLocationResponse_s3Bucket = Lens.lens (\CreateStorageLocationResponse' {s3Bucket} -> s3Bucket) (\s@CreateStorageLocationResponse' {} a -> s {s3Bucket = a} :: CreateStorageLocationResponse)

-- | The response's http status code.
createStorageLocationResponse_httpStatus :: Lens.Lens' CreateStorageLocationResponse Core.Int
createStorageLocationResponse_httpStatus = Lens.lens (\CreateStorageLocationResponse' {httpStatus} -> httpStatus) (\s@CreateStorageLocationResponse' {} a -> s {httpStatus = a} :: CreateStorageLocationResponse)

instance Core.NFData CreateStorageLocationResponse
