{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the switch for the specific directory to always use LDAP secure calls.
module Network.AWS.DirectoryService.EnableLDAPS
    (
    -- * Creating a request
      EnableLDAPS (..)
    , mkEnableLDAPS
    -- ** Request lenses
    , eldapsDirectoryId
    , eldapsType

    -- * Destructuring the response
    , EnableLDAPSResponse (..)
    , mkEnableLDAPSResponse
    -- ** Response lenses
    , eldapsrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , type' :: Types.LDAPSType
    -- ^ The type of LDAP security to enable. Currently only the value @Client@ is supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableLDAPS' value with any optional fields omitted.
mkEnableLDAPS
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.LDAPSType -- ^ 'type\''
    -> EnableLDAPS
mkEnableLDAPS directoryId type' = EnableLDAPS'{directoryId, type'}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsDirectoryId :: Lens.Lens' EnableLDAPS Types.DirectoryId
eldapsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE eldapsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsType :: Lens.Lens' EnableLDAPS Types.LDAPSType
eldapsType = Lens.field @"type'"
{-# INLINEABLE eldapsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery EnableLDAPS where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableLDAPS where
        toHeaders EnableLDAPS{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.EnableLDAPS")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableLDAPS where
        toJSON EnableLDAPS{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("Type" Core..= type')])

instance Core.AWSRequest EnableLDAPS where
        type Rs EnableLDAPS = EnableLDAPSResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableLDAPSResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableLDAPSResponse' smart constructor.
newtype EnableLDAPSResponse = EnableLDAPSResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableLDAPSResponse' value with any optional fields omitted.
mkEnableLDAPSResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableLDAPSResponse
mkEnableLDAPSResponse responseStatus
  = EnableLDAPSResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsrrsResponseStatus :: Lens.Lens' EnableLDAPSResponse Core.Int
eldapsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eldapsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
