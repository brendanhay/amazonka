{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.RequestUploadCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a fresh set of credentials for use when uploading a new set of game build files to Amazon GameLift's Amazon S3. This is done as part of the build creation process; see 'CreateBuild' .
--
-- To request new credentials, specify the build ID as returned with an initial @CreateBuild@ request. If successful, a new set of credentials are returned, along with the S3 storage location associated with the build ID.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build Create a Build with Files in S3> 
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
module Network.AWS.GameLift.RequestUploadCredentials
    (
    -- * Creating a request
      RequestUploadCredentials (..)
    , mkRequestUploadCredentials
    -- ** Request lenses
    , rucBuildId

    -- * Destructuring the response
    , RequestUploadCredentialsResponse (..)
    , mkRequestUploadCredentialsResponse
    -- ** Response lenses
    , rucrrsStorageLocation
    , rucrrsUploadCredentials
    , rucrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkRequestUploadCredentials' smart constructor.
newtype RequestUploadCredentials = RequestUploadCredentials'
  { buildId :: Types.BuildId
    -- ^ A unique identifier for a build to get credentials for. You can use either the build ID or ARN value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RequestUploadCredentials' value with any optional fields omitted.
mkRequestUploadCredentials
    :: Types.BuildId -- ^ 'buildId'
    -> RequestUploadCredentials
mkRequestUploadCredentials buildId
  = RequestUploadCredentials'{buildId}

-- | A unique identifier for a build to get credentials for. You can use either the build ID or ARN value. 
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucBuildId :: Lens.Lens' RequestUploadCredentials Types.BuildId
rucBuildId = Lens.field @"buildId"
{-# INLINEABLE rucBuildId #-}
{-# DEPRECATED buildId "Use generic-lens or generic-optics with 'buildId' instead"  #-}

instance Core.ToQuery RequestUploadCredentials where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RequestUploadCredentials where
        toHeaders RequestUploadCredentials{..}
          = Core.pure ("X-Amz-Target", "GameLift.RequestUploadCredentials")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RequestUploadCredentials where
        toJSON RequestUploadCredentials{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BuildId" Core..= buildId)])

instance Core.AWSRequest RequestUploadCredentials where
        type Rs RequestUploadCredentials = RequestUploadCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RequestUploadCredentialsResponse' Core.<$>
                   (x Core..:? "StorageLocation") Core.<*>
                     x Core..:? "UploadCredentials"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkRequestUploadCredentialsResponse' smart constructor.
data RequestUploadCredentialsResponse = RequestUploadCredentialsResponse'
  { storageLocation :: Core.Maybe Types.S3Location
    -- ^ Amazon S3 path and key, identifying where the game build files are stored.
  , uploadCredentials :: Core.Maybe Types.AwsCredentials
    -- ^ AWS credentials required when uploading a game build to the storage location. These credentials have a limited lifespan and are valid only for the build they were issued for.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestUploadCredentialsResponse' value with any optional fields omitted.
mkRequestUploadCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RequestUploadCredentialsResponse
mkRequestUploadCredentialsResponse responseStatus
  = RequestUploadCredentialsResponse'{storageLocation = Core.Nothing,
                                      uploadCredentials = Core.Nothing, responseStatus}

-- | Amazon S3 path and key, identifying where the game build files are stored.
--
-- /Note:/ Consider using 'storageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrrsStorageLocation :: Lens.Lens' RequestUploadCredentialsResponse (Core.Maybe Types.S3Location)
rucrrsStorageLocation = Lens.field @"storageLocation"
{-# INLINEABLE rucrrsStorageLocation #-}
{-# DEPRECATED storageLocation "Use generic-lens or generic-optics with 'storageLocation' instead"  #-}

-- | AWS credentials required when uploading a game build to the storage location. These credentials have a limited lifespan and are valid only for the build they were issued for.
--
-- /Note:/ Consider using 'uploadCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrrsUploadCredentials :: Lens.Lens' RequestUploadCredentialsResponse (Core.Maybe Types.AwsCredentials)
rucrrsUploadCredentials = Lens.field @"uploadCredentials"
{-# INLINEABLE rucrrsUploadCredentials #-}
{-# DEPRECATED uploadCredentials "Use generic-lens or generic-optics with 'uploadCredentials' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rucrrsResponseStatus :: Lens.Lens' RequestUploadCredentialsResponse Core.Int
rucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
