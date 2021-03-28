{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutDataCatalogEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the security configuration for a specified catalog. After the configuration has been set, the specified encryption is applied to every catalog write thereafter.
module Network.AWS.Glue.PutDataCatalogEncryptionSettings
    (
    -- * Creating a request
      PutDataCatalogEncryptionSettings (..)
    , mkPutDataCatalogEncryptionSettings
    -- ** Request lenses
    , pdcesDataCatalogEncryptionSettings
    , pdcesCatalogId

    -- * Destructuring the response
    , PutDataCatalogEncryptionSettingsResponse (..)
    , mkPutDataCatalogEncryptionSettingsResponse
    -- ** Response lenses
    , pdcesrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutDataCatalogEncryptionSettings' smart constructor.
data PutDataCatalogEncryptionSettings = PutDataCatalogEncryptionSettings'
  { dataCatalogEncryptionSettings :: Types.DataCatalogEncryptionSettings
    -- ^ The security configuration to set.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDataCatalogEncryptionSettings' value with any optional fields omitted.
mkPutDataCatalogEncryptionSettings
    :: Types.DataCatalogEncryptionSettings -- ^ 'dataCatalogEncryptionSettings'
    -> PutDataCatalogEncryptionSettings
mkPutDataCatalogEncryptionSettings dataCatalogEncryptionSettings
  = PutDataCatalogEncryptionSettings'{dataCatalogEncryptionSettings,
                                      catalogId = Core.Nothing}

-- | The security configuration to set.
--
-- /Note:/ Consider using 'dataCatalogEncryptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesDataCatalogEncryptionSettings :: Lens.Lens' PutDataCatalogEncryptionSettings Types.DataCatalogEncryptionSettings
pdcesDataCatalogEncryptionSettings = Lens.field @"dataCatalogEncryptionSettings"
{-# INLINEABLE pdcesDataCatalogEncryptionSettings #-}
{-# DEPRECATED dataCatalogEncryptionSettings "Use generic-lens or generic-optics with 'dataCatalogEncryptionSettings' instead"  #-}

-- | The ID of the Data Catalog to set the security configuration for. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesCatalogId :: Lens.Lens' PutDataCatalogEncryptionSettings (Core.Maybe Types.CatalogId)
pdcesCatalogId = Lens.field @"catalogId"
{-# INLINEABLE pdcesCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

instance Core.ToQuery PutDataCatalogEncryptionSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutDataCatalogEncryptionSettings where
        toHeaders PutDataCatalogEncryptionSettings{..}
          = Core.pure
              ("X-Amz-Target", "AWSGlue.PutDataCatalogEncryptionSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutDataCatalogEncryptionSettings where
        toJSON PutDataCatalogEncryptionSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("DataCatalogEncryptionSettings" Core..=
                       dataCatalogEncryptionSettings),
                  ("CatalogId" Core..=) Core.<$> catalogId])

instance Core.AWSRequest PutDataCatalogEncryptionSettings where
        type Rs PutDataCatalogEncryptionSettings =
             PutDataCatalogEncryptionSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutDataCatalogEncryptionSettingsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutDataCatalogEncryptionSettingsResponse' smart constructor.
newtype PutDataCatalogEncryptionSettingsResponse = PutDataCatalogEncryptionSettingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutDataCatalogEncryptionSettingsResponse' value with any optional fields omitted.
mkPutDataCatalogEncryptionSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutDataCatalogEncryptionSettingsResponse
mkPutDataCatalogEncryptionSettingsResponse responseStatus
  = PutDataCatalogEncryptionSettingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcesrrsResponseStatus :: Lens.Lens' PutDataCatalogEncryptionSettingsResponse Core.Int
pdcesrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pdcesrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
