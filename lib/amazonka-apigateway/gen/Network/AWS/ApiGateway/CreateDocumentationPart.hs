{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.CreateDocumentationPart
    (
    -- * Creating a request
      CreateDocumentationPart (..)
    , mkCreateDocumentationPart
    -- ** Request lenses
    , cdpRestApiId
    , cdpLocation
    , cdpProperties

     -- * Destructuring the response
    , Types.DocumentationPart (..)
    , Types.mkDocumentationPart
    -- ** Response lenses
    , Types.dpId
    , Types.dpLocation
    , Types.dpProperties
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new documentation part of a given API.
--
-- /See:/ 'mkCreateDocumentationPart' smart constructor.
data CreateDocumentationPart = CreateDocumentationPart'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , location :: Types.DocumentationPartLocation
    -- ^ [Required] The location of the targeted API entity of the to-be-created documentation part.
  , properties :: Core.Text
    -- ^ [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only OpenAPI-compliant key-value pairs can be exported and, hence, published.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDocumentationPart' value with any optional fields omitted.
mkCreateDocumentationPart
    :: Core.Text -- ^ 'restApiId'
    -> Types.DocumentationPartLocation -- ^ 'location'
    -> Core.Text -- ^ 'properties'
    -> CreateDocumentationPart
mkCreateDocumentationPart restApiId location properties
  = CreateDocumentationPart'{restApiId, location, properties}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRestApiId :: Lens.Lens' CreateDocumentationPart Core.Text
cdpRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cdpRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The location of the targeted API entity of the to-be-created documentation part.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpLocation :: Lens.Lens' CreateDocumentationPart Types.DocumentationPartLocation
cdpLocation = Lens.field @"location"
{-# INLINEABLE cdpLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | [Required] The new documentation content map of the targeted API entity. Enclosed key-value pairs are API-specific, but only OpenAPI-compliant key-value pairs can be exported and, hence, published.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpProperties :: Lens.Lens' CreateDocumentationPart Core.Text
cdpProperties = Lens.field @"properties"
{-# INLINEABLE cdpProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

instance Core.ToQuery CreateDocumentationPart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDocumentationPart where
        toHeaders CreateDocumentationPart{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateDocumentationPart where
        toJSON CreateDocumentationPart{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("location" Core..= location),
                  Core.Just ("properties" Core..= properties)])

instance Core.AWSRequest CreateDocumentationPart where
        type Rs CreateDocumentationPart = Types.DocumentationPart
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
