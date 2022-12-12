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
-- Module      : Amazonka.GameLift.CreateBuild
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build resource for your game server binary
-- files. Combine game server binaries into a zip file for use with Amazon
-- GameLift.
--
-- When setting up a new game build for GameLift, we recommend using the
-- CLI command
-- __<https://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html upload-build>__
-- . This helper command combines two tasks: (1) it uploads your build
-- files from a file directory to a GameLift Amazon S3 location, and (2) it
-- creates a new build resource.
--
-- You can use the operation in the following scenarios:
--
-- -   To create a new game build with build files that are in an Amazon S3
--     location under an Amazon Web Services account that you control. To
--     use this option, you give Amazon GameLift access to the Amazon S3
--     bucket. With permissions in place, specify a build name, operating
--     system, and the Amazon S3 storage location of your game build.
--
-- -   To directly upload your build files to a GameLift Amazon S3
--     location. To use this option, specify a build name and operating
--     system. This operation creates a new build resource and also returns
--     an Amazon S3 location with temporary access credentials. Use the
--     credentials to manually upload your build files to the specified
--     Amazon S3 location. For more information, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html Uploading Objects>
--     in the /Amazon S3 Developer Guide/. After you upload build files to
--     the GameLift Amazon S3 location, you can\'t update them.
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
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateBuild
  ( -- * Creating a Request
    CreateBuild (..),
    newCreateBuild,

    -- * Request Lenses
    createBuild_name,
    createBuild_operatingSystem,
    createBuild_serverSdkVersion,
    createBuild_storageLocation,
    createBuild_tags,
    createBuild_version,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBuild' smart constructor.
data CreateBuild = CreateBuild'
  { -- | A descriptive label associated with a build. Build names do not need to
    -- be unique. You can change this value later.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system that you built the game server binaries to run on.
    -- This value determines the type of fleet resources that you can use for
    -- this build. If your game build contains multiple executables, they all
    -- must run on the same operating system. If an operating system is not
    -- specified when creating a build, GameLift uses the default value
    -- (WINDOWS_2012). This value cannot be changed later.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A server SDK version you used when integrating your game server build
    -- with GameLift. For more information see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/integration-custom-intro.html Integrate games with custom game servers>.
    serverSdkVersion :: Prelude.Maybe Prelude.Text,
    -- | Information indicating where your game build files are stored. Use this
    -- parameter only when creating a build with files stored in an Amazon S3
    -- bucket that you own. The storage location must specify an Amazon S3
    -- bucket name and key. The location must also specify a role ARN that you
    -- set up to allow Amazon GameLift to access your Amazon S3 bucket. The S3
    -- bucket and your new build must be in the same Region.
    --
    -- If a @StorageLocation@ is specified, the size of your file can be found
    -- in your Amazon S3 bucket. Amazon GameLift will report a @SizeOnDisk@ of
    -- 0.
    storageLocation :: Prelude.Maybe S3Location,
    -- | A list of labels to assign to the new build resource. Tags are developer
    -- defined key-value pairs. Tagging Amazon Web Services resources are
    -- useful for resource management, access management and cost allocation.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/. Once the resource is
    -- created, you can use
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_TagResource.html TagResource>,
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UntagResource.html UntagResource>,
    -- and
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_ListTagsForResource.html ListTagsForResource>
    -- to add, remove, and view tags. The maximum tag limit may be lower than
    -- stated. See the Amazon Web Services General Reference for actual tagging
    -- limits.
    tags :: Prelude.Maybe [Tag],
    -- | Version information associated with a build or script. Version strings
    -- do not need to be unique. You can change this value later.
    version :: Prelude.Maybe Prelude.Text
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
-- 'name', 'createBuild_name' - A descriptive label associated with a build. Build names do not need to
-- be unique. You can change this value later.
--
-- 'operatingSystem', 'createBuild_operatingSystem' - The operating system that you built the game server binaries to run on.
-- This value determines the type of fleet resources that you can use for
-- this build. If your game build contains multiple executables, they all
-- must run on the same operating system. If an operating system is not
-- specified when creating a build, GameLift uses the default value
-- (WINDOWS_2012). This value cannot be changed later.
--
-- 'serverSdkVersion', 'createBuild_serverSdkVersion' - A server SDK version you used when integrating your game server build
-- with GameLift. For more information see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/integration-custom-intro.html Integrate games with custom game servers>.
--
-- 'storageLocation', 'createBuild_storageLocation' - Information indicating where your game build files are stored. Use this
-- parameter only when creating a build with files stored in an Amazon S3
-- bucket that you own. The storage location must specify an Amazon S3
-- bucket name and key. The location must also specify a role ARN that you
-- set up to allow Amazon GameLift to access your Amazon S3 bucket. The S3
-- bucket and your new build must be in the same Region.
--
-- If a @StorageLocation@ is specified, the size of your file can be found
-- in your Amazon S3 bucket. Amazon GameLift will report a @SizeOnDisk@ of
-- 0.
--
-- 'tags', 'createBuild_tags' - A list of labels to assign to the new build resource. Tags are developer
-- defined key-value pairs. Tagging Amazon Web Services resources are
-- useful for resource management, access management and cost allocation.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the resource is
-- created, you can use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_TagResource.html TagResource>,
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UntagResource.html UntagResource>,
-- and
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_ListTagsForResource.html ListTagsForResource>
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the Amazon Web Services General Reference for actual tagging
-- limits.
--
-- 'version', 'createBuild_version' - Version information associated with a build or script. Version strings
-- do not need to be unique. You can change this value later.
newCreateBuild ::
  CreateBuild
newCreateBuild =
  CreateBuild'
    { name = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      serverSdkVersion = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A descriptive label associated with a build. Build names do not need to
-- be unique. You can change this value later.
createBuild_name :: Lens.Lens' CreateBuild (Prelude.Maybe Prelude.Text)
createBuild_name = Lens.lens (\CreateBuild' {name} -> name) (\s@CreateBuild' {} a -> s {name = a} :: CreateBuild)

-- | The operating system that you built the game server binaries to run on.
-- This value determines the type of fleet resources that you can use for
-- this build. If your game build contains multiple executables, they all
-- must run on the same operating system. If an operating system is not
-- specified when creating a build, GameLift uses the default value
-- (WINDOWS_2012). This value cannot be changed later.
createBuild_operatingSystem :: Lens.Lens' CreateBuild (Prelude.Maybe OperatingSystem)
createBuild_operatingSystem = Lens.lens (\CreateBuild' {operatingSystem} -> operatingSystem) (\s@CreateBuild' {} a -> s {operatingSystem = a} :: CreateBuild)

-- | A server SDK version you used when integrating your game server build
-- with GameLift. For more information see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/integration-custom-intro.html Integrate games with custom game servers>.
createBuild_serverSdkVersion :: Lens.Lens' CreateBuild (Prelude.Maybe Prelude.Text)
createBuild_serverSdkVersion = Lens.lens (\CreateBuild' {serverSdkVersion} -> serverSdkVersion) (\s@CreateBuild' {} a -> s {serverSdkVersion = a} :: CreateBuild)

-- | Information indicating where your game build files are stored. Use this
-- parameter only when creating a build with files stored in an Amazon S3
-- bucket that you own. The storage location must specify an Amazon S3
-- bucket name and key. The location must also specify a role ARN that you
-- set up to allow Amazon GameLift to access your Amazon S3 bucket. The S3
-- bucket and your new build must be in the same Region.
--
-- If a @StorageLocation@ is specified, the size of your file can be found
-- in your Amazon S3 bucket. Amazon GameLift will report a @SizeOnDisk@ of
-- 0.
createBuild_storageLocation :: Lens.Lens' CreateBuild (Prelude.Maybe S3Location)
createBuild_storageLocation = Lens.lens (\CreateBuild' {storageLocation} -> storageLocation) (\s@CreateBuild' {} a -> s {storageLocation = a} :: CreateBuild)

-- | A list of labels to assign to the new build resource. Tags are developer
-- defined key-value pairs. Tagging Amazon Web Services resources are
-- useful for resource management, access management and cost allocation.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the resource is
-- created, you can use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_TagResource.html TagResource>,
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UntagResource.html UntagResource>,
-- and
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_ListTagsForResource.html ListTagsForResource>
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the Amazon Web Services General Reference for actual tagging
-- limits.
createBuild_tags :: Lens.Lens' CreateBuild (Prelude.Maybe [Tag])
createBuild_tags = Lens.lens (\CreateBuild' {tags} -> tags) (\s@CreateBuild' {} a -> s {tags = a} :: CreateBuild) Prelude.. Lens.mapping Lens.coerced

-- | Version information associated with a build or script. Version strings
-- do not need to be unique. You can change this value later.
createBuild_version :: Lens.Lens' CreateBuild (Prelude.Maybe Prelude.Text)
createBuild_version = Lens.lens (\CreateBuild' {version} -> version) (\s@CreateBuild' {} a -> s {version = a} :: CreateBuild)

instance Core.AWSRequest CreateBuild where
  type AWSResponse CreateBuild = CreateBuildResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBuildResponse'
            Prelude.<$> (x Data..?> "Build")
            Prelude.<*> (x Data..?> "StorageLocation")
            Prelude.<*> (x Data..?> "UploadCredentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBuild where
  hashWithSalt _salt CreateBuild' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` serverSdkVersion
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` version

instance Prelude.NFData CreateBuild where
  rnf CreateBuild' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf serverSdkVersion
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders CreateBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateBuild" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBuild where
  toJSON CreateBuild' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("OperatingSystem" Data..=)
              Prelude.<$> operatingSystem,
            ("ServerSdkVersion" Data..=)
              Prelude.<$> serverSdkVersion,
            ("StorageLocation" Data..=)
              Prelude.<$> storageLocation,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Version" Data..=) Prelude.<$> version
          ]
      )

instance Data.ToPath CreateBuild where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { -- | The newly created build resource, including a unique build IDs and
    -- status.
    build :: Prelude.Maybe Build,
    -- | Amazon S3 location for your game build file, including bucket name and
    -- key.
    storageLocation :: Prelude.Maybe S3Location,
    -- | This element is returned only when the operation is called without a
    -- storage location. It contains credentials to use when you are uploading
    -- a build file to an Amazon S3 bucket that is owned by Amazon GameLift.
    -- Credentials have a limited life span. To refresh these credentials, call
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RequestUploadCredentials.html RequestUploadCredentials>.
    uploadCredentials :: Prelude.Maybe (Data.Sensitive AwsCredentials),
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
-- a build file to an Amazon S3 bucket that is owned by Amazon GameLift.
-- Credentials have a limited life span. To refresh these credentials, call
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RequestUploadCredentials.html RequestUploadCredentials>.
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
-- a build file to an Amazon S3 bucket that is owned by Amazon GameLift.
-- Credentials have a limited life span. To refresh these credentials, call
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RequestUploadCredentials.html RequestUploadCredentials>.
createBuildResponse_uploadCredentials :: Lens.Lens' CreateBuildResponse (Prelude.Maybe AwsCredentials)
createBuildResponse_uploadCredentials = Lens.lens (\CreateBuildResponse' {uploadCredentials} -> uploadCredentials) (\s@CreateBuildResponse' {} a -> s {uploadCredentials = a} :: CreateBuildResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
createBuildResponse_httpStatus :: Lens.Lens' CreateBuildResponse Prelude.Int
createBuildResponse_httpStatus = Lens.lens (\CreateBuildResponse' {httpStatus} -> httpStatus) (\s@CreateBuildResponse' {} a -> s {httpStatus = a} :: CreateBuildResponse)

instance Prelude.NFData CreateBuildResponse where
  rnf CreateBuildResponse' {..} =
    Prelude.rnf build
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf uploadCredentials
      `Prelude.seq` Prelude.rnf httpStatus
