{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.GameLift.CreateBuild
    (
    -- * Creating a request
      CreateBuild (..)
    , mkCreateBuild
    -- ** Request lenses
    , cbName
    , cbOperatingSystem
    , cbStorageLocation
    , cbTags
    , cbVersion

    -- * Destructuring the response
    , CreateBuildResponse (..)
    , mkCreateBuildResponse
    -- ** Response lenses
    , cbrrsBuild
    , cbrrsStorageLocation
    , cbrrsUploadCredentials
    , cbrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateBuild' smart constructor.
data CreateBuild = CreateBuild'
  { name :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later. 
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
  , storageLocation :: Core.Maybe Types.S3Location
    -- ^ The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
  , version :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBuild' value with any optional fields omitted.
mkCreateBuild
    :: CreateBuild
mkCreateBuild
  = CreateBuild'{name = Core.Nothing, operatingSystem = Core.Nothing,
                 storageLocation = Core.Nothing, tags = Core.Nothing,
                 version = Core.Nothing}

-- | A descriptive label that is associated with a build. Build names do not need to be unique. You can use 'UpdateBuild' to change this value later. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbName :: Lens.Lens' CreateBuild (Core.Maybe Types.NonZeroAndMaxString)
cbName = Lens.field @"name"
{-# INLINEABLE cbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The operating system that the game server binaries are built to run on. This value determines the type of fleet resources that you can use for this build. If your game build contains multiple executables, they all must run on the same operating system. If an operating system is not specified when creating a build, Amazon GameLift uses the default value (WINDOWS_2012). This value cannot be changed later.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbOperatingSystem :: Lens.Lens' CreateBuild (Core.Maybe Types.OperatingSystem)
cbOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE cbOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

-- | The location where your game build files are stored. Use this parameter only when creating a build using files that are stored in an S3 bucket that you own. Identify an S3 bucket name and key, which must in the same Region where you're creating a build. This parameter must also specify the ARN for an IAM role that you've set up to give Amazon GameLift access your S3 bucket. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> . 
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbStorageLocation :: Lens.Lens' CreateBuild (Core.Maybe Types.S3Location)
cbStorageLocation = Lens.field @"storageLocation"
{-# INLINEABLE cbStorageLocation #-}
{-# DEPRECATED storageLocation "Use generic-lens or generic-optics with 'storageLocation' instead"  #-}

-- | A list of labels to assign to the new build resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBuild (Core.Maybe [Types.Tag])
cbTags = Lens.field @"tags"
{-# INLINEABLE cbTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateBuild' to change this value later. 
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbVersion :: Lens.Lens' CreateBuild (Core.Maybe Types.NonZeroAndMaxString)
cbVersion = Lens.field @"version"
{-# INLINEABLE cbVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery CreateBuild where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBuild where
        toHeaders CreateBuild{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateBuild") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateBuild where
        toJSON CreateBuild{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name,
                  ("OperatingSystem" Core..=) Core.<$> operatingSystem,
                  ("StorageLocation" Core..=) Core.<$> storageLocation,
                  ("Tags" Core..=) Core.<$> tags,
                  ("Version" Core..=) Core.<$> version])

instance Core.AWSRequest CreateBuild where
        type Rs CreateBuild = CreateBuildResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateBuildResponse' Core.<$>
                   (x Core..:? "Build") Core.<*> x Core..:? "StorageLocation" Core.<*>
                     x Core..:? "UploadCredentials"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateBuildResponse' smart constructor.
data CreateBuildResponse = CreateBuildResponse'
  { build :: Core.Maybe Types.Build
    -- ^ The newly created build resource, including a unique build IDs and status. 
  , storageLocation :: Core.Maybe Types.S3Location
    -- ^ Amazon S3 location for your game build file, including bucket name and key.
  , uploadCredentials :: Core.Maybe Types.AwsCredentials
    -- ^ This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateBuildResponse' value with any optional fields omitted.
mkCreateBuildResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBuildResponse
mkCreateBuildResponse responseStatus
  = CreateBuildResponse'{build = Core.Nothing,
                         storageLocation = Core.Nothing, uploadCredentials = Core.Nothing,
                         responseStatus}

-- | The newly created build resource, including a unique build IDs and status. 
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsBuild :: Lens.Lens' CreateBuildResponse (Core.Maybe Types.Build)
cbrrsBuild = Lens.field @"build"
{-# INLINEABLE cbrrsBuild #-}
{-# DEPRECATED build "Use generic-lens or generic-optics with 'build' instead"  #-}

-- | Amazon S3 location for your game build file, including bucket name and key.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsStorageLocation :: Lens.Lens' CreateBuildResponse (Core.Maybe Types.S3Location)
cbrrsStorageLocation = Lens.field @"storageLocation"
{-# INLINEABLE cbrrsStorageLocation #-}
{-# DEPRECATED storageLocation "Use generic-lens or generic-optics with 'storageLocation' instead"  #-}

-- | This element is returned only when the operation is called without a storage location. It contains credentials to use when you are uploading a build file to an S3 bucket that is owned by Amazon GameLift. Credentials have a limited life span. To refresh these credentials, call 'RequestUploadCredentials' . 
--
-- /Note:/ Consider using 'uploadCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsUploadCredentials :: Lens.Lens' CreateBuildResponse (Core.Maybe Types.AwsCredentials)
cbrrsUploadCredentials = Lens.field @"uploadCredentials"
{-# INLINEABLE cbrrsUploadCredentials #-}
{-# DEPRECATED uploadCredentials "Use generic-lens or generic-optics with 'uploadCredentials' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBuildResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
