{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.PutImageTagMutability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image tag mutability settings for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-tag-mutability.html Image Tag Mutability> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.PutImageTagMutability
    (
    -- * Creating a request
      PutImageTagMutability (..)
    , mkPutImageTagMutability
    -- ** Request lenses
    , pitmRepositoryName
    , pitmImageTagMutability
    , pitmRegistryId

    -- * Destructuring the response
    , PutImageTagMutabilityResponse (..)
    , mkPutImageTagMutabilityResponse
    -- ** Response lenses
    , pitmrrsImageTagMutability
    , pitmrrsRegistryId
    , pitmrrsRepositoryName
    , pitmrrsResponseStatus
    ) where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutImageTagMutability' smart constructor.
data PutImageTagMutability = PutImageTagMutability'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository in which to update the image tag mutability settings.
  , imageTagMutability :: Types.ImageTagMutability
    -- ^ The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageTagMutability' value with any optional fields omitted.
mkPutImageTagMutability
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.ImageTagMutability -- ^ 'imageTagMutability'
    -> PutImageTagMutability
mkPutImageTagMutability repositoryName imageTagMutability
  = PutImageTagMutability'{repositoryName, imageTagMutability,
                           registryId = Core.Nothing}

-- | The name of the repository in which to update the image tag mutability settings.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRepositoryName :: Lens.Lens' PutImageTagMutability Types.RepositoryName
pitmRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pitmRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmImageTagMutability :: Lens.Lens' PutImageTagMutability Types.ImageTagMutability
pitmImageTagMutability = Lens.field @"imageTagMutability"
{-# INLINEABLE pitmImageTagMutability #-}
{-# DEPRECATED imageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead"  #-}

-- | The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmRegistryId :: Lens.Lens' PutImageTagMutability (Core.Maybe Types.RegistryId)
pitmRegistryId = Lens.field @"registryId"
{-# INLINEABLE pitmRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

instance Core.ToQuery PutImageTagMutability where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutImageTagMutability where
        toHeaders PutImageTagMutability{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerRegistry_V20150921.PutImageTagMutability")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutImageTagMutability where
        toJSON PutImageTagMutability{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("imageTagMutability" Core..= imageTagMutability),
                  ("registryId" Core..=) Core.<$> registryId])

instance Core.AWSRequest PutImageTagMutability where
        type Rs PutImageTagMutability = PutImageTagMutabilityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutImageTagMutabilityResponse' Core.<$>
                   (x Core..:? "imageTagMutability") Core.<*> x Core..:? "registryId"
                     Core.<*> x Core..:? "repositoryName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutImageTagMutabilityResponse' smart constructor.
data PutImageTagMutabilityResponse = PutImageTagMutabilityResponse'
  { imageTagMutability :: Core.Maybe Types.ImageTagMutability
    -- ^ The image tag mutability setting for the repository.
  , registryId :: Core.Maybe Types.RegistryId
    -- ^ The registry ID associated with the request.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The repository name associated with the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutImageTagMutabilityResponse' value with any optional fields omitted.
mkPutImageTagMutabilityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutImageTagMutabilityResponse
mkPutImageTagMutabilityResponse responseStatus
  = PutImageTagMutabilityResponse'{imageTagMutability = Core.Nothing,
                                   registryId = Core.Nothing, repositoryName = Core.Nothing,
                                   responseStatus}

-- | The image tag mutability setting for the repository.
--
-- /Note:/ Consider using 'imageTagMutability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsImageTagMutability :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.ImageTagMutability)
pitmrrsImageTagMutability = Lens.field @"imageTagMutability"
{-# INLINEABLE pitmrrsImageTagMutability #-}
{-# DEPRECATED imageTagMutability "Use generic-lens or generic-optics with 'imageTagMutability' instead"  #-}

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsRegistryId :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.RegistryId)
pitmrrsRegistryId = Lens.field @"registryId"
{-# INLINEABLE pitmrrsRegistryId #-}
{-# DEPRECATED registryId "Use generic-lens or generic-optics with 'registryId' instead"  #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsRepositoryName :: Lens.Lens' PutImageTagMutabilityResponse (Core.Maybe Types.RepositoryName)
pitmrrsRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE pitmrrsRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitmrrsResponseStatus :: Lens.Lens' PutImageTagMutabilityResponse Core.Int
pitmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pitmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
