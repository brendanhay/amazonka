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
-- Module      : Network.AWS.GameLift.RequestUploadCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fresh set of credentials for use when uploading a new set of
-- game build files to Amazon GameLift\'s Amazon S3. This is done as part
-- of the build creation process; see CreateBuild.
--
-- To request new credentials, specify the build ID as returned with an
-- initial @CreateBuild@ request. If successful, a new set of credentials
-- are returned, along with the S3 storage location associated with the
-- build ID.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in S3>
--
-- __Related operations__
--
-- -   CreateBuild
--
-- -   ListBuilds
--
-- -   DescribeBuild
--
-- -   UpdateBuild
--
-- -   DeleteBuild
module Network.AWS.GameLift.RequestUploadCredentials
  ( -- * Creating a Request
    RequestUploadCredentials (..),
    newRequestUploadCredentials,

    -- * Request Lenses
    requestUploadCredentials_buildId,

    -- * Destructuring the Response
    RequestUploadCredentialsResponse (..),
    newRequestUploadCredentialsResponse,

    -- * Response Lenses
    requestUploadCredentialsResponse_storageLocation,
    requestUploadCredentialsResponse_uploadCredentials,
    requestUploadCredentialsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newRequestUploadCredentials' smart constructor.
data RequestUploadCredentials = RequestUploadCredentials'
  { -- | A unique identifier for a build to get credentials for. You can use
    -- either the build ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestUploadCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildId', 'requestUploadCredentials_buildId' - A unique identifier for a build to get credentials for. You can use
-- either the build ID or ARN value.
newRequestUploadCredentials ::
  -- | 'buildId'
  Prelude.Text ->
  RequestUploadCredentials
newRequestUploadCredentials pBuildId_ =
  RequestUploadCredentials' {buildId = pBuildId_}

-- | A unique identifier for a build to get credentials for. You can use
-- either the build ID or ARN value.
requestUploadCredentials_buildId :: Lens.Lens' RequestUploadCredentials Prelude.Text
requestUploadCredentials_buildId = Lens.lens (\RequestUploadCredentials' {buildId} -> buildId) (\s@RequestUploadCredentials' {} a -> s {buildId = a} :: RequestUploadCredentials)

instance Core.AWSRequest RequestUploadCredentials where
  type
    AWSResponse RequestUploadCredentials =
      RequestUploadCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RequestUploadCredentialsResponse'
            Prelude.<$> (x Core..?> "StorageLocation")
            Prelude.<*> (x Core..?> "UploadCredentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestUploadCredentials

instance Prelude.NFData RequestUploadCredentials

instance Core.ToHeaders RequestUploadCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.RequestUploadCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RequestUploadCredentials where
  toJSON RequestUploadCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("BuildId" Core..= buildId)]
      )

instance Core.ToPath RequestUploadCredentials where
  toPath = Prelude.const "/"

instance Core.ToQuery RequestUploadCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newRequestUploadCredentialsResponse' smart constructor.
data RequestUploadCredentialsResponse = RequestUploadCredentialsResponse'
  { -- | Amazon S3 path and key, identifying where the game build files are
    -- stored.
    storageLocation :: Prelude.Maybe S3Location,
    -- | AWS credentials required when uploading a game build to the storage
    -- location. These credentials have a limited lifespan and are valid only
    -- for the build they were issued for.
    uploadCredentials :: Prelude.Maybe (Core.Sensitive AwsCredentials),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestUploadCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageLocation', 'requestUploadCredentialsResponse_storageLocation' - Amazon S3 path and key, identifying where the game build files are
-- stored.
--
-- 'uploadCredentials', 'requestUploadCredentialsResponse_uploadCredentials' - AWS credentials required when uploading a game build to the storage
-- location. These credentials have a limited lifespan and are valid only
-- for the build they were issued for.
--
-- 'httpStatus', 'requestUploadCredentialsResponse_httpStatus' - The response's http status code.
newRequestUploadCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RequestUploadCredentialsResponse
newRequestUploadCredentialsResponse pHttpStatus_ =
  RequestUploadCredentialsResponse'
    { storageLocation =
        Prelude.Nothing,
      uploadCredentials = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon S3 path and key, identifying where the game build files are
-- stored.
requestUploadCredentialsResponse_storageLocation :: Lens.Lens' RequestUploadCredentialsResponse (Prelude.Maybe S3Location)
requestUploadCredentialsResponse_storageLocation = Lens.lens (\RequestUploadCredentialsResponse' {storageLocation} -> storageLocation) (\s@RequestUploadCredentialsResponse' {} a -> s {storageLocation = a} :: RequestUploadCredentialsResponse)

-- | AWS credentials required when uploading a game build to the storage
-- location. These credentials have a limited lifespan and are valid only
-- for the build they were issued for.
requestUploadCredentialsResponse_uploadCredentials :: Lens.Lens' RequestUploadCredentialsResponse (Prelude.Maybe AwsCredentials)
requestUploadCredentialsResponse_uploadCredentials = Lens.lens (\RequestUploadCredentialsResponse' {uploadCredentials} -> uploadCredentials) (\s@RequestUploadCredentialsResponse' {} a -> s {uploadCredentials = a} :: RequestUploadCredentialsResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
requestUploadCredentialsResponse_httpStatus :: Lens.Lens' RequestUploadCredentialsResponse Prelude.Int
requestUploadCredentialsResponse_httpStatus = Lens.lens (\RequestUploadCredentialsResponse' {httpStatus} -> httpStatus) (\s@RequestUploadCredentialsResponse' {} a -> s {httpStatus = a} :: RequestUploadCredentialsResponse)

instance
  Prelude.NFData
    RequestUploadCredentialsResponse
