{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified security profiles to the specified user.
module Network.AWS.Connect.UpdateUserSecurityProfiles
  ( -- * Creating a request
    UpdateUserSecurityProfiles (..),
    mkUpdateUserSecurityProfiles,

    -- ** Request lenses
    uuspSecurityProfileIds,
    uuspUserId,
    uuspInstanceId,

    -- * Destructuring the response
    UpdateUserSecurityProfilesResponse (..),
    mkUpdateUserSecurityProfilesResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Core.NonEmpty Types.SecurityProfileId,
    -- | The identifier of the user account.
    userId :: Types.UserId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserSecurityProfiles' value with any optional fields omitted.
mkUpdateUserSecurityProfiles ::
  -- | 'securityProfileIds'
  Core.NonEmpty Types.SecurityProfileId ->
  -- | 'userId'
  Types.UserId ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserSecurityProfiles
mkUpdateUserSecurityProfiles securityProfileIds userId instanceId =
  UpdateUserSecurityProfiles'
    { securityProfileIds,
      userId,
      instanceId
    }

-- | The identifiers of the security profiles for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspSecurityProfileIds :: Lens.Lens' UpdateUserSecurityProfiles (Core.NonEmpty Types.SecurityProfileId)
uuspSecurityProfileIds = Lens.field @"securityProfileIds"
{-# DEPRECATED uuspSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspUserId :: Lens.Lens' UpdateUserSecurityProfiles Types.UserId
uuspUserId = Lens.field @"userId"
{-# DEPRECATED uuspUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspInstanceId :: Lens.Lens' UpdateUserSecurityProfiles Types.InstanceId
uuspInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uuspInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserSecurityProfiles where
  toJSON UpdateUserSecurityProfiles {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SecurityProfileIds" Core..= securityProfileIds)]
      )

instance Core.AWSRequest UpdateUserSecurityProfiles where
  type
    Rs UpdateUserSecurityProfiles =
      UpdateUserSecurityProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/users/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText userId)
                Core.<> ("/security-profiles")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateUserSecurityProfilesResponse'

-- | /See:/ 'mkUpdateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserSecurityProfilesResponse' value with any optional fields omitted.
mkUpdateUserSecurityProfilesResponse ::
  UpdateUserSecurityProfilesResponse
mkUpdateUserSecurityProfilesResponse =
  UpdateUserSecurityProfilesResponse'
