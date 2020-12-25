{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity information for the specified user.
--
-- /Important:/ Someone with the ability to invoke @UpdateUserIndentityInfo@ can change the login credentials of other users by changing their email address. This poses a security risk to your organization. They can change the email address of a user to the attacker's email address, and then reset the password through email. We strongly recommend limiting who has the ability to invoke @UpdateUserIndentityInfo@ . For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-best-practices.html Best Practices for Security Profiles> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.UpdateUserIdentityInfo
  ( -- * Creating a request
    UpdateUserIdentityInfo (..),
    mkUpdateUserIdentityInfo,

    -- ** Request lenses
    uuiiIdentityInfo,
    uuiiUserId,
    uuiiInstanceId,

    -- * Destructuring the response
    UpdateUserIdentityInfoResponse (..),
    mkUpdateUserIdentityInfoResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserIdentityInfo' smart constructor.
data UpdateUserIdentityInfo = UpdateUserIdentityInfo'
  { -- | The identity information for the user.
    identityInfo :: Types.UserIdentityInfo,
    -- | The identifier of the user account.
    userId :: Types.UserId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserIdentityInfo' value with any optional fields omitted.
mkUpdateUserIdentityInfo ::
  -- | 'identityInfo'
  Types.UserIdentityInfo ->
  -- | 'userId'
  Types.UserId ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserIdentityInfo
mkUpdateUserIdentityInfo identityInfo userId instanceId =
  UpdateUserIdentityInfo' {identityInfo, userId, instanceId}

-- | The identity information for the user.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiIdentityInfo :: Lens.Lens' UpdateUserIdentityInfo Types.UserIdentityInfo
uuiiIdentityInfo = Lens.field @"identityInfo"
{-# DEPRECATED uuiiIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiUserId :: Lens.Lens' UpdateUserIdentityInfo Types.UserId
uuiiUserId = Lens.field @"userId"
{-# DEPRECATED uuiiUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiInstanceId :: Lens.Lens' UpdateUserIdentityInfo Types.InstanceId
uuiiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uuiiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserIdentityInfo where
  toJSON UpdateUserIdentityInfo {..} =
    Core.object
      (Core.catMaybes [Core.Just ("IdentityInfo" Core..= identityInfo)])

instance Core.AWSRequest UpdateUserIdentityInfo where
  type Rs UpdateUserIdentityInfo = UpdateUserIdentityInfoResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/users/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText userId)
                Core.<> ("/identity-info")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateUserIdentityInfoResponse'

-- | /See:/ 'mkUpdateUserIdentityInfoResponse' smart constructor.
data UpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserIdentityInfoResponse' value with any optional fields omitted.
mkUpdateUserIdentityInfoResponse ::
  UpdateUserIdentityInfoResponse
mkUpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
