{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateUserSecurityProfiles (..)
    , mkUpdateUserSecurityProfiles
    -- ** Request lenses
    , uuspSecurityProfileIds
    , uuspUserId
    , uuspInstanceId

    -- * Destructuring the response
    , UpdateUserSecurityProfilesResponse (..)
    , mkUpdateUserSecurityProfilesResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { securityProfileIds :: Core.NonEmpty Types.SecurityProfileId
    -- ^ The identifiers of the security profiles for the user.
  , userId :: Types.UserId
    -- ^ The identifier of the user account.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserSecurityProfiles' value with any optional fields omitted.
mkUpdateUserSecurityProfiles
    :: Core.NonEmpty Types.SecurityProfileId -- ^ 'securityProfileIds'
    -> Types.UserId -- ^ 'userId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> UpdateUserSecurityProfiles
mkUpdateUserSecurityProfiles securityProfileIds userId instanceId
  = UpdateUserSecurityProfiles'{securityProfileIds, userId,
                                instanceId}

-- | The identifiers of the security profiles for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspSecurityProfileIds :: Lens.Lens' UpdateUserSecurityProfiles (Core.NonEmpty Types.SecurityProfileId)
uuspSecurityProfileIds = Lens.field @"securityProfileIds"
{-# INLINEABLE uuspSecurityProfileIds #-}
{-# DEPRECATED securityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead"  #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspUserId :: Lens.Lens' UpdateUserSecurityProfiles Types.UserId
uuspUserId = Lens.field @"userId"
{-# INLINEABLE uuspUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspInstanceId :: Lens.Lens' UpdateUserSecurityProfiles Types.InstanceId
uuspInstanceId = Lens.field @"instanceId"
{-# INLINEABLE uuspInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery UpdateUserSecurityProfiles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserSecurityProfiles where
        toHeaders UpdateUserSecurityProfiles{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserSecurityProfiles where
        toJSON UpdateUserSecurityProfiles{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecurityProfileIds" Core..= securityProfileIds)])

instance Core.AWSRequest UpdateUserSecurityProfiles where
        type Rs UpdateUserSecurityProfiles =
             UpdateUserSecurityProfilesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/users/" Core.<> Core.toText instanceId Core.<> "/" Core.<>
                             Core.toText userId
                             Core.<> "/security-profiles",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateUserSecurityProfilesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserSecurityProfilesResponse' value with any optional fields omitted.
mkUpdateUserSecurityProfilesResponse
    :: UpdateUserSecurityProfilesResponse
mkUpdateUserSecurityProfilesResponse
  = UpdateUserSecurityProfilesResponse'
