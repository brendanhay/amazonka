{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon GameLift build resource for your game server binary files. Game server binaries must be combined into a zip file for use with Amazon GameLift.
--
-- /Important:/ When setting up a new game build for GameLift, we recommend using the AWS CLI command __<https://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html upload-build> __ . This helper command combines two tasks: (1) it uploads your build files from a file directory to a GameLift Amazon S3 location, and (2) it creates a new build resource.
-- The @CreateBuild@ operation can used in the following scenarios:
--
--     * To create a new game build with build files that are in an S3 location under an AWS account that you control. To use this option, you must first give Amazon GameLift access to the S3 bucket. With permissions in place, call @CreateBuild@ and specify a build name, operating system, and the S3 storage location of your game build.
--
--
--     * To directly upload your build files to a GameLift S3 location. To use this option, first call @CreateBuild@ and specify a build name and operating system. This operation creates a new build resource and also returns an S3 location with temporary access credentials. Use the credentials to manually upload your build files to the specified S3 location. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html Uploading Objects> in the /Amazon S3 Developer Guide/ . Build files can be uploaded to the GameLift S3 location once only; that can't be updated.
--
--
-- If successful, this operation creates a new build resource with a unique build ID and places it in @INITIALIZED@ status. A build must be in @READY@ status before you can create fleets with it.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Uploading Your Game>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in Amazon S3>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
module Network.AWS.GameLift.CreateBuild
  ( -- * Creating a request
    CreateBuild (..),
    mkCreateBuild,

    -- ** Request lenses
    cbStorageLocation,
    cbOperatingSystem,
    cbName,
    cbVersion,
    cbTags,

    -- * Destructuring the response
    CreateBuildResponse (..),
    mkCreateBuildResponse,

    -- ** Response lenses
    cbrsStorageLocation,
    cbrsUploadCredentials,
    cbrsBuild,
    cbrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateBuild' smart constructor.
data CreateBuild = CreateBuild'
  { -- | The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
    storageLocation :: Lude.Maybe S3Location,
    -- | The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
    name :: Lude.Maybe Lude.Text,
    -- | Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
    version :: Lude.Maybe Lude.Text,
    -- | A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBuild' with the minimum fields required to make a request.
--
-- * 'storageLocation' - The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
-- * 'operatingSystem' - The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
-- * 'name' - A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
-- * 'version' - Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
-- * 'tags' - A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
mkCreateBuild ::
  CreateBuild
mkCreateBuild =
  CreateBuild'
    { storageLocation = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> .
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbStorageLocation :: Lens.Lens' CreateBuild (Lude.Maybe S3Location)
cbStorageLocation = Lens.lens (storageLocation :: CreateBuild -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: CreateBuild)
{-# DEPRECATED cbStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbOperatingSystem :: Lens.Lens' CreateBuild (Lude.Maybe OperatingSystem)
cbOperatingSystem = Lens.lens (operatingSystem :: CreateBuild -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: CreateBuild)
{-# DEPRECATED cbOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbName :: Lens.Lens' CreateBuild (Lude.Maybe Lude.Text)
cbName = Lens.lens (name :: CreateBuild -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateBuild)
{-# DEPRECATED cbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbVersion :: Lens.Lens' CreateBuild (Lude.Maybe Lude.Text)
cbVersion = Lens.lens (version :: CreateBuild -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: CreateBuild)
{-# DEPRECATED cbVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBuild (Lude.Maybe [Tag])
cbTags = Lens.lens (tags :: CreateBuild -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateBuild)
{-# DEPRECATED cbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateBuild where
  type Rs CreateBuild = CreateBuildResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBuildResponse'
            Lude.<$> (x Lude..?> "StorageLocation")
            Lude.<*> (x Lude..?> "UploadCredentials")
            Lude.<*> (x Lude..?> "Build")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBuild where
  toJSON CreateBuild' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StorageLocation" Lude..=) Lude.<$> storageLocation,
            ("OperatingSystem" Lude..=) Lude.<$> operatingSystem,
            ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBuild where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { -- | Amazon S3 location for your game build file, including bucket name and key.
    storageLocation :: Lude.Maybe S3Location,
    -- | This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
    uploadCredentials :: Lude.Maybe AWSCredentials,
    -- | The newly created build resource, including a unique build IDs and status.
    build :: Lude.Maybe Build,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBuildResponse' with the minimum fields required to make a request.
--
-- * 'storageLocation' - Amazon S3 location for your game build file, including bucket name and key.
-- * 'uploadCredentials' - This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
-- * 'build' - The newly created build resource, including a unique build IDs and status.
-- * 'responseStatus' - The response status code.
mkCreateBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBuildResponse
mkCreateBuildResponse pResponseStatus_ =
  CreateBuildResponse'
    { storageLocation = Lude.Nothing,
      uploadCredentials = Lude.Nothing,
      build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Amazon S3 location for your game build file, including bucket name and key.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsStorageLocation :: Lens.Lens' CreateBuildResponse (Lude.Maybe S3Location)
cbrsStorageLocation = Lens.lens (storageLocation :: CreateBuildResponse -> Lude.Maybe S3Location) (\s a -> s {storageLocation = a} :: CreateBuildResponse)
{-# DEPRECATED cbrsStorageLocation "Use generic-lens or generic-optics with 'storageLocation' instead." #-}

-- | This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' .
--
-- /Note:/ Consider using 'uploadCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsUploadCredentials :: Lens.Lens' CreateBuildResponse (Lude.Maybe AWSCredentials)
cbrsUploadCredentials = Lens.lens (uploadCredentials :: CreateBuildResponse -> Lude.Maybe AWSCredentials) (\s a -> s {uploadCredentials = a} :: CreateBuildResponse)
{-# DEPRECATED cbrsUploadCredentials "Use generic-lens or generic-optics with 'uploadCredentials' instead." #-}

-- | The newly created build resource, including a unique build IDs and status.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsBuild :: Lens.Lens' CreateBuildResponse (Lude.Maybe Build)
cbrsBuild = Lens.lens (build :: CreateBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: CreateBuildResponse)
{-# DEPRECATED cbrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBuildResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBuildResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
