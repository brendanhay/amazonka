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
-- Module      : Network.AWS.GameLift.CreateBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build resource for your game server binary
-- files. Game server binaries must be combined into a zip file for use
-- with Amazon GameLift.
--
-- When setting up a new game build for GameLift, we recommend using the
-- AWS CLI command
-- __<https://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html upload-build>__
-- . This helper command combines two tasks: (1) it uploads your build
-- files from a file directory to a GameLift Amazon S3 location, and (2) it
-- creates a new build resource.
--
-- The @CreateBuild@ operation can used in the following scenarios:
--
-- -   To create a new game build with build files that are in an S3
--     location under an AWS account that you control. To use this option,
--     you must first give Amazon GameLift access to the S3 bucket. With
--     permissions in place, call @CreateBuild@ and specify a build name,
--     operating system, and the S3 storage location of your game build.
--
-- -   To directly upload your build files to a GameLift S3 location. To
--     use this option, first call @CreateBuild@ and specify a build name
--     and operating system. This operation creates a new build resource
--     and also returns an S3 location with temporary access credentials.
--     Use the credentials to manually upload your build files to the
--     specified S3 location. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html Uploading Objects>
--     in the /Amazon S3 Developer Guide/. Build files can be uploaded to
--     the GameLift S3 location once only; that can\'t be updated.
--
-- If successful, this operation creates a new build resource with a unique
-- build ID and places it in @INITIALIZED@ status. A build must be in
-- @READY@ status before you can create fleets with it.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Uploading Your Game>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3>
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
module Network.AWS.GameLift.CreateBuild
  ( -- * Creating a Request
    CreateBuild (..),
    newCreateBuild,

    -- * Request Lenses
    createBuild_version,
    createBuild_name,
    createBuild_storageLocation,
    createBuild_tags,
    createBuild_operatingSystem,

    -- * Destructuring the Response
    CreateBuildResponse (..),
    newCreateBuildResponse,

    -- * Response Lenses
    createBuildResponse_build,
    createBuildResponse_storageLocation,
    createBuildResponse_uploadCredentials,
    createBuildResponse_httpStatus,
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
-- /See:/ 'newCreateBuild' smart constructor.
data CreateBuild = CreateBuild'
  { -- | Version information that is associated with a build or script. Version
    -- strings do not need to be unique. You can use UpdateBuild to change this
    -- value later.
    version :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a build. Build names do not
    -- need to be unique. You can use UpdateBuild to change this value later.
    name :: Prelude.Maybe Prelude.Text,
    -- | The location where your game build files are stored. Use this parameter
    -- only when creating a build using files that are stored in an S3 bucket
    -- that you own. Identify an S3 bucket name and key, which must in the same
    -- Region where you\'re creating a build. This parameter must also specify
    -- the ARN for an IAM role that you\'ve set up to give Amazon GameLift
    -- access your S3 bucket. To call this operation with a storage location,
    -- you must have IAM PassRole permission. For more details on IAM roles and
    -- PassRole permissions, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
    storageLocation :: Prelude.Maybe S3Location,
    -- | A list of labels to assign to the new build resource. Tags are
    -- developer-defined key-value pairs. Tagging AWS resources are useful for
    -- resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Prelude.Maybe [Tag],
    -- | The operating system that the game server binaries are built to run on.
    -- This value determines the type of fleet resources that you can use for
    -- this build. If your game build contains multiple executables, they all
    -- must run on the same operating system. If an operating system is not
    -- specified when creating a build, Amazon GameLift uses the default value
    -- (WINDOWS_2012). This value cannot be changed later.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'createBuild_version' - Version information that is associated with a build or script. Version
-- strings do not need to be unique. You can use UpdateBuild to change this
-- value later.
--
-- 'name', 'createBuild_name' - A descriptive label that is associated with a build. Build names do not
-- need to be unique. You can use UpdateBuild to change this value later.
--
-- 'storageLocation', 'createBuild_storageLocation' - The location where your game build files are stored. Use this parameter
-- only when creating a build using files that are stored in an S3 bucket
-- that you own. Identify an S3 bucket name and key, which must in the same
-- Region where you\'re creating a build. This parameter must also specify
-- the ARN for an IAM role that you\'ve set up to give Amazon GameLift
-- access your S3 bucket. To call this operation with a storage location,
-- you must have IAM PassRole permission. For more details on IAM roles and
-- PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
--
-- 'tags', 'createBuild_tags' - A list of labels to assign to the new build resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'operatingSystem', 'createBuild_operatingSystem' - The operating system that the game server binaries are built to run on.
-- This value determines the type of fleet resources that you can use for
-- this build. If your game build contains multiple executables, they all
-- must run on the same operating system. If an operating system is not
-- specified when creating a build, Amazon GameLift uses the default value
-- (WINDOWS_2012). This value cannot be changed later.
newCreateBuild ::
  CreateBuild
newCreateBuild =
  CreateBuild'
    { version = Prelude.Nothing,
      name = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      tags = Prelude.Nothing,
      operatingSystem = Prelude.Nothing
    }

-- | Version information that is associated with a build or script. Version
-- strings do not need to be unique. You can use UpdateBuild to change this
-- value later.
createBuild_version :: Lens.Lens' CreateBuild (Prelude.Maybe Prelude.Text)
createBuild_version = Lens.lens (\CreateBuild' {version} -> version) (\s@CreateBuild' {} a -> s {version = a} :: CreateBuild)

-- | A descriptive label that is associated with a build. Build names do not
-- need to be unique. You can use UpdateBuild to change this value later.
createBuild_name :: Lens.Lens' CreateBuild (Prelude.Maybe Prelude.Text)
createBuild_name = Lens.lens (\CreateBuild' {name} -> name) (\s@CreateBuild' {} a -> s {name = a} :: CreateBuild)

-- | The location where your game build files are stored. Use this parameter
-- only when creating a build using files that are stored in an S3 bucket
-- that you own. Identify an S3 bucket name and key, which must in the same
-- Region where you\'re creating a build. This parameter must also specify
-- the ARN for an IAM role that you\'ve set up to give Amazon GameLift
-- access your S3 bucket. To call this operation with a storage location,
-- you must have IAM PassRole permission. For more details on IAM roles and
-- PassRole permissions, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access>.
createBuild_storageLocation :: Lens.Lens' CreateBuild (Prelude.Maybe S3Location)
createBuild_storageLocation = Lens.lens (\CreateBuild' {storageLocation} -> storageLocation) (\s@CreateBuild' {} a -> s {storageLocation = a} :: CreateBuild)

-- | A list of labels to assign to the new build resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createBuild_tags :: Lens.Lens' CreateBuild (Prelude.Maybe [Tag])
createBuild_tags = Lens.lens (\CreateBuild' {tags} -> tags) (\s@CreateBuild' {} a -> s {tags = a} :: CreateBuild) Prelude.. Lens.mapping Lens._Coerce

-- | The operating system that the game server binaries are built to run on.
-- This value determines the type of fleet resources that you can use for
-- this build. If your game build contains multiple executables, they all
-- must run on the same operating system. If an operating system is not
-- specified when creating a build, Amazon GameLift uses the default value
-- (WINDOWS_2012). This value cannot be changed later.
createBuild_operatingSystem :: Lens.Lens' CreateBuild (Prelude.Maybe OperatingSystem)
createBuild_operatingSystem = Lens.lens (\CreateBuild' {operatingSystem} -> operatingSystem) (\s@CreateBuild' {} a -> s {operatingSystem = a} :: CreateBuild)

instance Core.AWSRequest CreateBuild where
  type AWSResponse CreateBuild = CreateBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBuildResponse'
            Prelude.<$> (x Core..?> "Build")
            Prelude.<*> (x Core..?> "StorageLocation")
            Prelude.<*> (x Core..?> "UploadCredentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBuild

instance Prelude.NFData CreateBuild

instance Core.ToHeaders CreateBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreateBuild" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBuild where
  toJSON CreateBuild' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Version" Core..=) Prelude.<$> version,
            ("Name" Core..=) Prelude.<$> name,
            ("StorageLocation" Core..=)
              Prelude.<$> storageLocation,
            ("Tags" Core..=) Prelude.<$> tags,
            ("OperatingSystem" Core..=)
              Prelude.<$> operatingSystem
          ]
      )

instance Core.ToPath CreateBuild where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBuild where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { -- | The newly created build resource, including a unique build IDs and
    -- status.
    build :: Prelude.Maybe Build,
    -- | Amazon S3 location for your game build file, including bucket name and
    -- key.
    storageLocation :: Prelude.Maybe S3Location,
    -- | This element is returned only when the operation is called without a
    -- storage location. It contains credentials to use when you are uploading
    -- a build file to an S3 bucket that is owned by Amazon GameLift.
    -- Credentials have a limited life span. To refresh these credentials, call
    -- RequestUploadCredentials.
    uploadCredentials :: Prelude.Maybe (Core.Sensitive AwsCredentials),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'createBuildResponse_build' - The newly created build resource, including a unique build IDs and
-- status.
--
-- 'storageLocation', 'createBuildResponse_storageLocation' - Amazon S3 location for your game build file, including bucket name and
-- key.
--
-- 'uploadCredentials', 'createBuildResponse_uploadCredentials' - This element is returned only when the operation is called without a
-- storage location. It contains credentials to use when you are uploading
-- a build file to an S3 bucket that is owned by Amazon GameLift.
-- Credentials have a limited life span. To refresh these credentials, call
-- RequestUploadCredentials.
--
-- 'httpStatus', 'createBuildResponse_httpStatus' - The response's http status code.
newCreateBuildResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBuildResponse
newCreateBuildResponse pHttpStatus_ =
  CreateBuildResponse'
    { build = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      uploadCredentials = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created build resource, including a unique build IDs and
-- status.
createBuildResponse_build :: Lens.Lens' CreateBuildResponse (Prelude.Maybe Build)
createBuildResponse_build = Lens.lens (\CreateBuildResponse' {build} -> build) (\s@CreateBuildResponse' {} a -> s {build = a} :: CreateBuildResponse)

-- | Amazon S3 location for your game build file, including bucket name and
-- key.
createBuildResponse_storageLocation :: Lens.Lens' CreateBuildResponse (Prelude.Maybe S3Location)
createBuildResponse_storageLocation = Lens.lens (\CreateBuildResponse' {storageLocation} -> storageLocation) (\s@CreateBuildResponse' {} a -> s {storageLocation = a} :: CreateBuildResponse)

-- | This element is returned only when the operation is called without a
-- storage location. It contains credentials to use when you are uploading
-- a build file to an S3 bucket that is owned by Amazon GameLift.
-- Credentials have a limited life span. To refresh these credentials, call
-- RequestUploadCredentials.
createBuildResponse_uploadCredentials :: Lens.Lens' CreateBuildResponse (Prelude.Maybe AwsCredentials)
createBuildResponse_uploadCredentials = Lens.lens (\CreateBuildResponse' {uploadCredentials} -> uploadCredentials) (\s@CreateBuildResponse' {} a -> s {uploadCredentials = a} :: CreateBuildResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
createBuildResponse_httpStatus :: Lens.Lens' CreateBuildResponse Prelude.Int
createBuildResponse_httpStatus = Lens.lens (\CreateBuildResponse' {httpStatus} -> httpStatus) (\s@CreateBuildResponse' {} a -> s {httpStatus = a} :: CreateBuildResponse)

instance Prelude.NFData CreateBuildResponse
