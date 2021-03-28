{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.ImportApiKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import API keys from an external source, such as a CSV-formatted file.
module Network.AWS.ApiGateway.ImportApiKeys
    (
    -- * Creating a request
      ImportApiKeys (..)
    , mkImportApiKeys
    -- ** Request lenses
    , iakBody
    , iakFormat
    , iakFailOnWarnings

    -- * Destructuring the response
    , ImportApiKeysResponse (..)
    , mkImportApiKeysResponse
    -- ** Response lenses
    , iakrrsIds
    , iakrrsWarnings
    , iakrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST request to import API keys from an external source, such as a CSV-formatted file.
--
-- /See:/ 'mkImportApiKeys' smart constructor.
data ImportApiKeys = ImportApiKeys'
  { body :: Core.ByteString
    -- ^ The payload of the POST request to import API keys. For the payload format, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format> .
  , format :: Types.ApiKeysFormat
    -- ^ A query parameter to specify the input format to imported API keys. Currently, only the @csv@ format is supported.
  , failOnWarnings :: Core.Maybe Core.Bool
    -- ^ A query parameter to indicate whether to rollback 'ApiKey' importation (@true@ ) or not (@false@ ) when error is encountered.
  }
  deriving stock (Core.Eq, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportApiKeys' value with any optional fields omitted.
mkImportApiKeys
    :: Core.ByteString -- ^ 'body'
    -> Types.ApiKeysFormat -- ^ 'format'
    -> ImportApiKeys
mkImportApiKeys body format
  = ImportApiKeys'{body, format, failOnWarnings = Core.Nothing}

-- | The payload of the POST request to import API keys. For the payload format, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakBody :: Lens.Lens' ImportApiKeys Core.ByteString
iakBody = Lens.field @"body"
{-# INLINEABLE iakBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | A query parameter to specify the input format to imported API keys. Currently, only the @csv@ format is supported.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakFormat :: Lens.Lens' ImportApiKeys Types.ApiKeysFormat
iakFormat = Lens.field @"format"
{-# INLINEABLE iakFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | A query parameter to indicate whether to rollback 'ApiKey' importation (@true@ ) or not (@false@ ) when error is encountered.
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakFailOnWarnings :: Lens.Lens' ImportApiKeys (Core.Maybe Core.Bool)
iakFailOnWarnings = Lens.field @"failOnWarnings"
{-# INLINEABLE iakFailOnWarnings #-}
{-# DEPRECATED failOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead"  #-}

instance Core.ToQuery ImportApiKeys where
        toQuery ImportApiKeys{..}
          = Core.toQueryPair "format" format Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "failonwarnings")
                failOnWarnings
              Core.<> Core.toQueryPair "mode=import" ("" :: Core.Text)

instance Core.ToHeaders ImportApiKeys where
        toHeaders ImportApiKeys{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest ImportApiKeys where
        type Rs ImportApiKeys = ImportApiKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/apikeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportApiKeysResponse' Core.<$>
                   (x Core..:? "ids") Core.<*> x Core..:? "warnings" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The identifier of an 'ApiKey' used in a 'UsagePlan' .
--
-- /See:/ 'mkImportApiKeysResponse' smart constructor.
data ImportApiKeysResponse = ImportApiKeysResponse'
  { ids :: Core.Maybe [Core.Text]
    -- ^ A list of all the 'ApiKey' identifiers.
  , warnings :: Core.Maybe [Core.Text]
    -- ^ A list of warning messages.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportApiKeysResponse' value with any optional fields omitted.
mkImportApiKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportApiKeysResponse
mkImportApiKeysResponse responseStatus
  = ImportApiKeysResponse'{ids = Core.Nothing,
                           warnings = Core.Nothing, responseStatus}

-- | A list of all the 'ApiKey' identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrrsIds :: Lens.Lens' ImportApiKeysResponse (Core.Maybe [Core.Text])
iakrrsIds = Lens.field @"ids"
{-# INLINEABLE iakrrsIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

-- | A list of warning messages.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrrsWarnings :: Lens.Lens' ImportApiKeysResponse (Core.Maybe [Core.Text])
iakrrsWarnings = Lens.field @"warnings"
{-# INLINEABLE iakrrsWarnings #-}
{-# DEPRECATED warnings "Use generic-lens or generic-optics with 'warnings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrrsResponseStatus :: Lens.Lens' ImportApiKeysResponse Core.Int
iakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
