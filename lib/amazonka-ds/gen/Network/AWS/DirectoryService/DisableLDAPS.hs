{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates LDAP secure calls for the specified directory.
module Network.AWS.DirectoryService.DisableLDAPS
    (
    -- * Creating a request
      DisableLDAPS (..)
    , mkDisableLDAPS
    -- ** Request lenses
    , dldapsDirectoryId
    , dldapsType

    -- * Destructuring the response
    , DisableLDAPSResponse (..)
    , mkDisableLDAPSResponse
    -- ** Response lenses
    , dldapsrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , type' :: Types.LDAPSType
    -- ^ The type of LDAP security to enable. Currently only the value @Client@ is supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableLDAPS' value with any optional fields omitted.
mkDisableLDAPS
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.LDAPSType -- ^ 'type\''
    -> DisableLDAPS
mkDisableLDAPS directoryId type'
  = DisableLDAPS'{directoryId, type'}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsDirectoryId :: Lens.Lens' DisableLDAPS Types.DirectoryId
dldapsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dldapsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsType :: Lens.Lens' DisableLDAPS Types.LDAPSType
dldapsType = Lens.field @"type'"
{-# INLINEABLE dldapsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery DisableLDAPS where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisableLDAPS where
        toHeaders DisableLDAPS{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DisableLDAPS")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisableLDAPS where
        toJSON DisableLDAPS{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("Type" Core..= type')])

instance Core.AWSRequest DisableLDAPS where
        type Rs DisableLDAPS = DisableLDAPSResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisableLDAPSResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableLDAPSResponse' smart constructor.
newtype DisableLDAPSResponse = DisableLDAPSResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableLDAPSResponse' value with any optional fields omitted.
mkDisableLDAPSResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableLDAPSResponse
mkDisableLDAPSResponse responseStatus
  = DisableLDAPSResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsrrsResponseStatus :: Lens.Lens' DisableLDAPSResponse Core.Int
dldapsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dldapsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
