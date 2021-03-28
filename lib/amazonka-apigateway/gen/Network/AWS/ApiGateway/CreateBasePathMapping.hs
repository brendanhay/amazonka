{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'BasePathMapping' resource.
module Network.AWS.ApiGateway.CreateBasePathMapping
    (
    -- * Creating a request
      CreateBasePathMapping (..)
    , mkCreateBasePathMapping
    -- ** Request lenses
    , cbpmDomainName
    , cbpmRestApiId
    , cbpmBasePath
    , cbpmStage

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

-- | Requests API Gateway to create a new 'BasePathMapping' resource.
--
-- /See:/ 'mkCreateBasePathMapping' smart constructor.
data CreateBasePathMapping = CreateBasePathMapping'
  { domainName :: Core.Text
    -- ^ [Required] The domain name of the 'BasePathMapping' resource to create.
  , restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , basePath :: Core.Maybe Core.Text
    -- ^ The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify a base path name after the domain name.
  , stage :: Core.Maybe Core.Text
    -- ^ The name of the API's stage that you want to use for this mapping. Specify '(none)' if you want callers to explicitly specify the stage name after any base path name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBasePathMapping' value with any optional fields omitted.
mkCreateBasePathMapping
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'restApiId'
    -> CreateBasePathMapping
mkCreateBasePathMapping domainName restApiId
  = CreateBasePathMapping'{domainName, restApiId,
                           basePath = Core.Nothing, stage = Core.Nothing}

-- | [Required] The domain name of the 'BasePathMapping' resource to create.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmDomainName :: Lens.Lens' CreateBasePathMapping Core.Text
cbpmDomainName = Lens.field @"domainName"
{-# INLINEABLE cbpmDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmRestApiId :: Lens.Lens' CreateBasePathMapping Core.Text
cbpmRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cbpmRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify a base path name after the domain name.
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmBasePath :: Lens.Lens' CreateBasePathMapping (Core.Maybe Core.Text)
cbpmBasePath = Lens.field @"basePath"
{-# INLINEABLE cbpmBasePath #-}
{-# DEPRECATED basePath "Use generic-lens or generic-optics with 'basePath' instead"  #-}

-- | The name of the API's stage that you want to use for this mapping. Specify '(none)' if you want callers to explicitly specify the stage name after any base path name.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmStage :: Lens.Lens' CreateBasePathMapping (Core.Maybe Core.Text)
cbpmStage = Lens.field @"stage"
{-# INLINEABLE cbpmStage #-}
{-# DEPRECATED stage "Use generic-lens or generic-optics with 'stage' instead"  #-}

instance Core.ToQuery CreateBasePathMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBasePathMapping where
        toHeaders CreateBasePathMapping{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateBasePathMapping where
        toJSON CreateBasePathMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("restApiId" Core..= restApiId),
                  ("basePath" Core..=) Core.<$> basePath,
                  ("stage" Core..=) Core.<$> stage])

instance Core.AWSRequest CreateBasePathMapping where
        type Rs CreateBasePathMapping = Types.BasePathMapping
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/domainnames/" Core.<> Core.toText domainName Core.<>
                             "/basepathmappings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
