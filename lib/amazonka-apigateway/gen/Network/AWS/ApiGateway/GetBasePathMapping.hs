{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a 'BasePathMapping' resource.
module Network.AWS.ApiGateway.GetBasePathMapping
    (
    -- * Creating a request
      GetBasePathMapping (..)
    , mkGetBasePathMapping
    -- ** Request lenses
    , gbpmDomainName
    , gbpmBasePath

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

-- | Request to describe a 'BasePathMapping' resource.
--
-- /See:/ 'mkGetBasePathMapping' smart constructor.
data GetBasePathMapping = GetBasePathMapping'
  { domainName :: Core.Text
    -- ^ [Required] The domain name of the 'BasePathMapping' resource to be described.
  , basePath :: Core.Text
    -- ^ [Required] The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify any base path name after the domain name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBasePathMapping' value with any optional fields omitted.
mkGetBasePathMapping
    :: Core.Text -- ^ 'domainName'
    -> Core.Text -- ^ 'basePath'
    -> GetBasePathMapping
mkGetBasePathMapping domainName basePath
  = GetBasePathMapping'{domainName, basePath}

-- | [Required] The domain name of the 'BasePathMapping' resource to be described.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmDomainName :: Lens.Lens' GetBasePathMapping Core.Text
gbpmDomainName = Lens.field @"domainName"
{-# INLINEABLE gbpmDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | [Required] The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify any base path name after the domain name.
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmBasePath :: Lens.Lens' GetBasePathMapping Core.Text
gbpmBasePath = Lens.field @"basePath"
{-# INLINEABLE gbpmBasePath #-}
{-# DEPRECATED basePath "Use generic-lens or generic-optics with 'basePath' instead"  #-}

instance Core.ToQuery GetBasePathMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBasePathMapping where
        toHeaders GetBasePathMapping{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetBasePathMapping where
        type Rs GetBasePathMapping = Types.BasePathMapping
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/domainnames/" Core.<> Core.toText domainName Core.<>
                             "/basepathmappings/"
                             Core.<> Core.toText basePath,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
