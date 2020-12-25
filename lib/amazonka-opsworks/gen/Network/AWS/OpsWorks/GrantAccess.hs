{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.GrantAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants RDP access to a Windows instance for a specified time period.
module Network.AWS.OpsWorks.GrantAccess
  ( -- * Creating a request
    GrantAccess (..),
    mkGrantAccess,

    -- ** Request lenses
    gaInstanceId,
    gaValidForInMinutes,

    -- * Destructuring the response
    GrantAccessResponse (..),
    mkGrantAccessResponse,

    -- ** Response lenses
    garrsTemporaryCredential,
    garrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGrantAccess' smart constructor.
data GrantAccess = GrantAccess'
  { -- | The instance's AWS OpsWorks Stacks ID.
    instanceId :: Types.String,
    -- | The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
    validForInMinutes :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GrantAccess' value with any optional fields omitted.
mkGrantAccess ::
  -- | 'instanceId'
  Types.String ->
  GrantAccess
mkGrantAccess instanceId =
  GrantAccess' {instanceId, validForInMinutes = Core.Nothing}

-- | The instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaInstanceId :: Lens.Lens' GrantAccess Types.String
gaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
--
-- /Note:/ Consider using 'validForInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaValidForInMinutes :: Lens.Lens' GrantAccess (Core.Maybe Core.Natural)
gaValidForInMinutes = Lens.field @"validForInMinutes"
{-# DEPRECATED gaValidForInMinutes "Use generic-lens or generic-optics with 'validForInMinutes' instead." #-}

instance Core.FromJSON GrantAccess where
  toJSON GrantAccess {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            ("ValidForInMinutes" Core..=) Core.<$> validForInMinutes
          ]
      )

instance Core.AWSRequest GrantAccess where
  type Rs GrantAccess = GrantAccessResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.GrantAccess")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GrantAccessResponse'
            Core.<$> (x Core..:? "TemporaryCredential")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @GrantAccess@ request.
--
-- /See:/ 'mkGrantAccessResponse' smart constructor.
data GrantAccessResponse = GrantAccessResponse'
  { -- | A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
    temporaryCredential :: Core.Maybe Types.TemporaryCredential,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GrantAccessResponse' value with any optional fields omitted.
mkGrantAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GrantAccessResponse
mkGrantAccessResponse responseStatus =
  GrantAccessResponse'
    { temporaryCredential = Core.Nothing,
      responseStatus
    }

-- | A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
--
-- /Note:/ Consider using 'temporaryCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsTemporaryCredential :: Lens.Lens' GrantAccessResponse (Core.Maybe Types.TemporaryCredential)
garrsTemporaryCredential = Lens.field @"temporaryCredential"
{-# DEPRECATED garrsTemporaryCredential "Use generic-lens or generic-optics with 'temporaryCredential' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GrantAccessResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
