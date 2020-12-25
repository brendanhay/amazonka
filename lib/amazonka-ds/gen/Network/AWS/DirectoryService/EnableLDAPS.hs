{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EnableLDAPS (..),
    mkEnableLDAPS,

    -- ** Request lenses
    eldapsDirectoryId,
    eldapsType,

    -- * Destructuring the response
    EnableLDAPSResponse (..),
    mkEnableLDAPSResponse,

    -- ** Response lenses
    eldapsrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
    type' :: Types.LDAPSType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableLDAPS' value with any optional fields omitted.
mkEnableLDAPS ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'type\''
  Types.LDAPSType ->
  EnableLDAPS
mkEnableLDAPS directoryId type' = EnableLDAPS' {directoryId, type'}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsDirectoryId :: Lens.Lens' EnableLDAPS Types.DirectoryId
eldapsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED eldapsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsType :: Lens.Lens' EnableLDAPS Types.LDAPSType
eldapsType = Lens.field @"type'"
{-# DEPRECATED eldapsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON EnableLDAPS where
  toJSON EnableLDAPS {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.AWSRequest EnableLDAPS where
  type Rs EnableLDAPS = EnableLDAPSResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DirectoryService_20150416.EnableLDAPS")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableLDAPSResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableLDAPSResponse' smart constructor.
newtype EnableLDAPSResponse = EnableLDAPSResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableLDAPSResponse' value with any optional fields omitted.
mkEnableLDAPSResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableLDAPSResponse
mkEnableLDAPSResponse responseStatus =
  EnableLDAPSResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsrrsResponseStatus :: Lens.Lens' EnableLDAPSResponse Core.Int
eldapsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eldapsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
