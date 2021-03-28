{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, @CreateStorageLocation@ still returns the bucket name but does not create a new bucket.
module Network.AWS.ElasticBeanstalk.CreateStorageLocation
    (
    -- * Creating a request
      CreateStorageLocation (..)
    , mkCreateStorageLocation

    -- * Destructuring the response
    , CreateStorageLocationResponse (..)
    , mkCreateStorageLocationResponse
    -- ** Response lenses
    , cslrrsS3Bucket
    , cslrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStorageLocation' smart constructor.
data CreateStorageLocation = CreateStorageLocation'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStorageLocation' value with any optional fields omitted.
mkCreateStorageLocation
    :: CreateStorageLocation
mkCreateStorageLocation = CreateStorageLocation'

instance Core.ToQuery CreateStorageLocation where
        toQuery CreateStorageLocation{..}
          = Core.toQueryPair "Action" ("CreateStorageLocation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders CreateStorageLocation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateStorageLocation where
        type Rs CreateStorageLocation = CreateStorageLocationResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateStorageLocationResult"
              (\ s h x ->
                 CreateStorageLocationResponse' Core.<$>
                   (x Core..@? "S3Bucket") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Results of a 'CreateStorageLocationResult' call.
--
-- /See:/ 'mkCreateStorageLocationResponse' smart constructor.
data CreateStorageLocationResponse = CreateStorageLocationResponse'
  { s3Bucket :: Core.Maybe Types.S3Bucket
    -- ^ The name of the Amazon S3 bucket created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStorageLocationResponse' value with any optional fields omitted.
mkCreateStorageLocationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStorageLocationResponse
mkCreateStorageLocationResponse responseStatus
  = CreateStorageLocationResponse'{s3Bucket = Core.Nothing,
                                   responseStatus}

-- | The name of the Amazon S3 bucket created.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrsS3Bucket :: Lens.Lens' CreateStorageLocationResponse (Core.Maybe Types.S3Bucket)
cslrrsS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE cslrrsS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrsResponseStatus :: Lens.Lens' CreateStorageLocationResponse Core.Int
cslrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cslrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
