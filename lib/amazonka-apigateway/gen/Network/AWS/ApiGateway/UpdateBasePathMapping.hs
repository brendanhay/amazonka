{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'BasePathMapping' resource.
module Network.AWS.ApiGateway.UpdateBasePathMapping
    (
    -- * Creating a request
      UpdateBasePathMapping (..)
    , mkUpdateBasePathMapping
    -- ** Request lenses
    , ubpmDomainName
    , ubpmBasePath
    , ubpmPatchOperations

     -- * Destructuring the response
    , Types.BasePathMapping (..)
    , Types.mkBasePathMapping
    -- ** Response lenses
    , Types.bpmBasePath
    , Types.bpmRestApiId
    , Types.bpmStage
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about the 'BasePathMapping' resource.
--
-- /See:/ 'mkUpdateBasePathMapping' smart constructor.
data UpdateBasePathMapping = UpdateBasePathMapping'
  { domainName :: Core.Text
    -- ^ [Required] The domain name of the 'BasePathMapping' resource to change.
  , basePath :: Core.Text
    -- ^ [Required] The base path of the 'BasePathMapping' resource to change.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBasePathMapping' value with any optional fields omitted.
mkUpdateBasePathMapping
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'basePath'
    -> UpdateBasePathMapping
mkUpdateBasePathMapping domainName basePath
  = UpdateBasePathMapping'{domainName, basePath,
                           patchOperations = Core.Nothing}

-- | [Required] The domain name of the 'BasePathMapping' resource to change.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmDomainName :: Lens.Lens' UpdateBasePathMapping Core.Text
ubpmDomainName = Lens.field @"domainName"
{-# INLINEABLE ubpmDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | [Required] The base path of the 'BasePathMapping' resource to change.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmBasePath :: Lens.Lens' UpdateBasePathMapping Core.Text
ubpmBasePath = Lens.field @"basePath"
{-# INLINEABLE ubpmBasePath #-}
{-# DEPRECATED basePath "Use generic-lens or generic-optics with 'basePath' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmPatchOperations :: Lens.Lens' UpdateBasePathMapping (Core.Maybe [Types.PatchOperation])
ubpmPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE ubpmPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateBasePathMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBasePathMapping where
        toHeaders UpdateBasePathMapping{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateBasePathMapping where
        toJSON UpdateBasePathMapping{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateBasePathMapping where
        type Rs UpdateBasePathMapping = Types.BasePathMapping
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/domainnames/" Core.<> Core.toText domainName Core.<>
                             "/basepathmappings/"
                             Core.<> Core.toText basePath,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
