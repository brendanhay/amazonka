{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.GetDocumentationPart
    (
    -- * Creating a request
      GetDocumentationPart (..)
    , mkGetDocumentationPart
    -- ** Request lenses
    , gdpfRestApiId
    , gdpfDocumentationPartId

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

-- | Gets a specified documentation part of a given API.
--
-- /See:/ 'mkGetDocumentationPart' smart constructor.
data GetDocumentationPart = GetDocumentationPart'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationPartId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentationPart' value with any optional fields omitted.
mkGetDocumentationPart
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationPartId'
    -> GetDocumentationPart
mkGetDocumentationPart restApiId documentationPartId
  = GetDocumentationPart'{restApiId, documentationPartId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpfRestApiId :: Lens.Lens' GetDocumentationPart Core.Text
gdpfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gdpfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpfDocumentationPartId :: Lens.Lens' GetDocumentationPart Core.Text
gdpfDocumentationPartId = Lens.field @"documentationPartId"
{-# INLINEABLE gdpfDocumentationPartId #-}
{-# DEPRECATED documentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead"  #-}

instance Core.ToQuery GetDocumentationPart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDocumentationPart where
        toHeaders GetDocumentationPart{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDocumentationPart where
        type Rs GetDocumentationPart = Types.DocumentationPart
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts/"
                             Core.<> Core.toText documentationPartId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
