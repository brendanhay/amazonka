{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.GetDocumentationVersion
    (
    -- * Creating a request
      GetDocumentationVersion (..)
    , mkGetDocumentationVersion
    -- ** Request lenses
    , gdvfRestApiId
    , gdvfDocumentationVersion

     -- * Destructuring the response
    , Types.DocumentationVersion (..)
    , Types.mkDocumentationVersion
    -- ** Response lenses
    , Types.dvCreatedDate
    , Types.dvDescription
    , Types.dvVersion
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a documentation snapshot of an API.
--
-- /See:/ 'mkGetDocumentationVersion' smart constructor.
data GetDocumentationVersion = GetDocumentationVersion'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationVersion :: Core.Text
    -- ^ [Required] The version identifier of the to-be-retrieved documentation snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentationVersion' value with any optional fields omitted.
mkGetDocumentationVersion
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationVersion'
    -> GetDocumentationVersion
mkGetDocumentationVersion restApiId documentationVersion
  = GetDocumentationVersion'{restApiId, documentationVersion}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvfRestApiId :: Lens.Lens' GetDocumentationVersion Core.Text
gdvfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gdvfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The version identifier of the to-be-retrieved documentation snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvfDocumentationVersion :: Lens.Lens' GetDocumentationVersion Core.Text
gdvfDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE gdvfDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

instance Core.ToQuery GetDocumentationVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDocumentationVersion where
        toHeaders GetDocumentationVersion{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDocumentationVersion where
        type Rs GetDocumentationVersion = Types.DocumentationVersion
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/versions/"
                             Core.<> Core.toText documentationVersion,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
