{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.ImportDocumentationParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.ImportDocumentationParts
    (
    -- * Creating a request
      ImportDocumentationParts (..)
    , mkImportDocumentationParts
    -- ** Request lenses
    , idpRestApiId
    , idpBody
    , idpFailOnWarnings
    , idpMode

    -- * Destructuring the response
    , ImportDocumentationPartsResponse (..)
    , mkImportDocumentationPartsResponse
    -- ** Response lenses
    , idprrsIds
    , idprrsWarnings
    , idprrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Import documentation parts from an external (e.g., OpenAPI) definition file. 
--
-- /See:/ 'mkImportDocumentationParts' smart constructor.
data ImportDocumentationParts = ImportDocumentationParts'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , body :: Core.ByteString
    -- ^ [Required] Raw byte array representing the to-be-imported documentation parts. To import from an OpenAPI file, this is a JSON object.
  , failOnWarnings :: Core.Maybe Core.Bool
    -- ^ A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
  , mode :: Core.Maybe Types.PutMode
    -- ^ A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportDocumentationParts' value with any optional fields omitted.
mkImportDocumentationParts
    :: Core.Text -- ^ 'restApiId'
    -> Core.ByteString -- ^ 'body'
    -> ImportDocumentationParts
mkImportDocumentationParts restApiId body
  = ImportDocumentationParts'{restApiId, body,
                              failOnWarnings = Core.Nothing, mode = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpRestApiId :: Lens.Lens' ImportDocumentationParts Core.Text
idpRestApiId = Lens.field @"restApiId"
{-# INLINEABLE idpRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Raw byte array representing the to-be-imported documentation parts. To import from an OpenAPI file, this is a JSON object.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpBody :: Lens.Lens' ImportDocumentationParts Core.ByteString
idpBody = Lens.field @"body"
{-# INLINEABLE idpBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A query parameter to specify whether to rollback the documentation importation (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpFailOnWarnings :: Lens.Lens' ImportDocumentationParts (Core.Maybe Core.Bool)
idpFailOnWarnings = Lens.field @"failOnWarnings"
{-# INLINEABLE idpFailOnWarnings #-}
{-# DEPRECATED failOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead"  #-}

-- | A query parameter to indicate whether to overwrite (@OVERWRITE@ ) any existing 'DocumentationParts' definition or to merge (@MERGE@ ) the new definition into the existing one. The default value is @MERGE@ .
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idpMode :: Lens.Lens' ImportDocumentationParts (Core.Maybe Types.PutMode)
idpMode = Lens.field @"mode"
{-# INLINEABLE idpMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

instance Core.ToQuery ImportDocumentationParts where
        toQuery ImportDocumentationParts{..}
          = Core.maybe Core.mempty (Core.toQueryPair "failonwarnings")
              failOnWarnings
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "mode") mode

instance Core.ToHeaders ImportDocumentationParts where
        toHeaders ImportDocumentationParts{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest ImportDocumentationParts where
        type Rs ImportDocumentationParts = ImportDocumentationPartsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportDocumentationPartsResponse' Core.<$>
                   (x Core..:? "ids") Core.<*> x Core..:? "warnings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A collection of the imported 'DocumentationPart' identifiers.
--
-- This is used to return the result when documentation parts in an external (e.g., OpenAPI) file are imported into API Gateway<https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , <https://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/ documentationpart:import> , 'DocumentationPart' 
--
-- /See:/ 'mkImportDocumentationPartsResponse' smart constructor.
data ImportDocumentationPartsResponse = ImportDocumentationPartsResponse'
  { ids :: Core.Maybe [Core.Text]
    -- ^ A list of the returned documentation part identifiers.
  , warnings :: Core.Maybe [Core.Text]
    -- ^ A list of warning messages reported during import of documentation parts.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportDocumentationPartsResponse' value with any optional fields omitted.
mkImportDocumentationPartsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportDocumentationPartsResponse
mkImportDocumentationPartsResponse responseStatus
  = ImportDocumentationPartsResponse'{ids = Core.Nothing,
                                      warnings = Core.Nothing, responseStatus}

-- | A list of the returned documentation part identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprrsIds :: Lens.Lens' ImportDocumentationPartsResponse (Core.Maybe [Core.Text])
idprrsIds = Lens.field @"ids"
{-# INLINEABLE idprrsIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

-- | A list of warning messages reported during import of documentation parts.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprrsWarnings :: Lens.Lens' ImportDocumentationPartsResponse (Core.Maybe [Core.Text])
idprrsWarnings = Lens.field @"warnings"
{-# INLINEABLE idprrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idprrsResponseStatus :: Lens.Lens' ImportDocumentationPartsResponse Core.Int
idprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE idprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
