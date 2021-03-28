{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new script record for your Realtime Servers script. Realtime scripts are JavaScript that provide configuration settings and optional custom game logic for your game. The script is deployed when you create a Realtime Servers fleet to host your game sessions. Script logic is executed during an active game session. 
--
-- To create a new script record, specify a script name and provide the script file(s). The script files and all dependencies must be zipped into a single file. You can pull the zip file from either of these locations: 
--
--     * A locally available directory. Use the /ZipFile/ parameter for this option.
--
--
--     * An Amazon Simple Storage Service (Amazon S3) bucket under your AWS account. Use the /StorageLocation/ parameter for this option. You'll need to have an Identity Access Management (IAM) role that allows the Amazon GameLift service to access your S3 bucket. 
--
--
-- If the call is successful, a new script record is created with a unique script ID. If the script file is provided as a local file, the file is uploaded to an Amazon GameLift-owned S3 bucket and the script record's storage location reflects this location. If the script file is provided as an S3 bucket, Amazon GameLift accesses the file at this storage location as needed for deployment.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers> 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set Up a Role for Amazon GameLift Access> 
-- __Related operations__ 
--
--     * 'CreateScript' 
--
--
--     * 'ListScripts' 
--
--
--     * 'DescribeScript' 
--
--
--     * 'UpdateScript' 
--
--
--     * 'DeleteScript' 
--
--
module Network.AWS.GameLift.CreateScript
    (
    -- * Creating a request
      CreateScript (..)
    , mkCreateScript
    -- ** Request lenses
    , csName
    , csStorageLocation
    , csTags
    , csVersion
    , csZipFile

    -- * Destructuring the response
    , CreateScriptResponse (..)
    , mkCreateScriptResponse
    -- ** Response lenses
    , csrrsScript
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateScript' smart constructor.
data CreateScript = CreateScript'
  { name :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later. 
  , storageLocation :: Core.Maybe Types.S3Location
    -- ^ The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
  , version :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later. 
  , zipFile :: Core.Maybe Core.Base64
    -- ^ A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScript' value with any optional fields omitted.
mkCreateScript
    :: CreateScript
mkCreateScript
  = CreateScript'{name = Core.Nothing,
                  storageLocation = Core.Nothing, tags = Core.Nothing,
                  version = Core.Nothing, zipFile = Core.Nothing}

-- | A descriptive label that is associated with a script. Script names do not need to be unique. You can use 'UpdateScript' to change this value later. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateScript (Core.Maybe Types.NonZeroAndMaxString)
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon S3 location of your Realtime scripts. The storage location must specify the S3 bucket name, the zip file name (the "key"), and an IAM role ARN that allows Amazon GameLift to access the S3 storage location. The S3 bucket must be in the same Region where you are creating a new script. By default, Amazon GameLift uploads the latest version of the zip file; if you have S3 object versioning turned on, you can use the @ObjectVersion@ parameter to specify an earlier version. To call this operation with a storage location, you must have IAM PassRole permission. For more details on IAM roles and PassRole permissions, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/setting-up-role.html Set up a role for GameLift access> . 
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStorageLocation :: Lens.Lens' CreateScript (Core.Maybe Types.S3Location)
csStorageLocation = Lens.field @"storageLocation"
{-# INLINEABLE csStorageLocation #-}
{-# DEPRECATED storageLocation "Use generic-lens or generic-optics with 'storageLocation' instead"  #-}

-- | A list of labels to assign to the new script resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateScript (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# INLINEABLE csTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The version that is associated with a build or script. Version strings do not need to be unique. You can use 'UpdateScript' to change this value later. 
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVersion :: Lens.Lens' CreateScript (Core.Maybe Types.NonZeroAndMaxString)
csVersion = Lens.field @"version"
{-# INLINEABLE csVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | A data object containing your Realtime scripts and dependencies as a zip file. The zip file can have one or multiple files. Maximum size of a zip file is 5 MB.
--
-- When using the AWS CLI tool to create a script, this parameter is set to the zip file name. It must be prepended with the string "fileb://" to indicate that the file data is a binary object. For example: @--zip-file fileb://myRealtimeScript.zip@ .--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csZipFile :: Lens.Lens' CreateScript (Core.Maybe Core.Base64)
csZipFile = Lens.field @"zipFile"
{-# INLINEABLE csZipFile #-}
{-# DEPRECATED zipFile "Use generic-lens or generic-optics with 'zipFile' instead"  #-}

instance Core.ToQuery CreateScript where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateScript where
        toHeaders CreateScript{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateScript") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateScript where
        toJSON CreateScript{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name,
                  ("StorageLocation" Core..=) Core.<$> storageLocation,
                  ("Tags" Core..=) Core.<$> tags,
                  ("Version" Core..=) Core.<$> version,
                  ("ZipFile" Core..=) Core.<$> zipFile])

instance Core.AWSRequest CreateScript where
        type Rs CreateScript = CreateScriptResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateScriptResponse' Core.<$>
                   (x Core..:? "Script") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { script :: Core.Maybe Types.Script
    -- ^ The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateScriptResponse' value with any optional fields omitted.
mkCreateScriptResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateScriptResponse
mkCreateScriptResponse responseStatus
  = CreateScriptResponse'{script = Core.Nothing, responseStatus}

-- | The newly created script record with a unique script ID and ARN. The new script's storage location reflects an Amazon S3 location: (1) If the script was uploaded from an S3 bucket under your account, the storage location reflects the information that was provided in the /CreateScript/ request; (2) If the script file was uploaded from a local zip file, the storage location reflects an S3 location controls by the Amazon GameLift service.
--
-- /Note:/ Consider using 'script' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsScript :: Lens.Lens' CreateScriptResponse (Core.Maybe Types.Script)
csrrsScript = Lens.field @"script"
{-# INLINEABLE csrrsScript #-}
{-# DEPRECATED script "Use generic-lens or generic-optics with 'script' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateScriptResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
