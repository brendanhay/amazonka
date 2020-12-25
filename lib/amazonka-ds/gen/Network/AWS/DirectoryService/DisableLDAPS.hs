{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisableLDAPS (..),
    mkDisableLDAPS,

    -- ** Request lenses
    dldapsDirectoryId,
    dldapsType,

    -- * Destructuring the response
    DisableLDAPSResponse (..),
    mkDisableLDAPSResponse,

    -- ** Response lenses
    dldapsrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
    type' :: Types.LDAPSType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableLDAPS' value with any optional fields omitted.
mkDisableLDAPS ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'type\''
  Types.LDAPSType ->
  DisableLDAPS
mkDisableLDAPS directoryId type' =
  DisableLDAPS' {directoryId, type'}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsDirectoryId :: Lens.Lens' DisableLDAPS Types.DirectoryId
dldapsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dldapsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsType :: Lens.Lens' DisableLDAPS Types.LDAPSType
dldapsType = Lens.field @"type'"
{-# DEPRECATED dldapsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DisableLDAPS where
  toJSON DisableLDAPS {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.AWSRequest DisableLDAPS where
  type Rs DisableLDAPS = DisableLDAPSResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DisableLDAPS")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableLDAPSResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableLDAPSResponse' smart constructor.
newtype DisableLDAPSResponse = DisableLDAPSResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableLDAPSResponse' value with any optional fields omitted.
mkDisableLDAPSResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableLDAPSResponse
mkDisableLDAPSResponse responseStatus =
  DisableLDAPSResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsrrsResponseStatus :: Lens.Lens' DisableLDAPSResponse Core.Int
dldapsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dldapsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
