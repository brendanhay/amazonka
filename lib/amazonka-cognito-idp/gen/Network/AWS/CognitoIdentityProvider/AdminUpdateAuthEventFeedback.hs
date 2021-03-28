{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.
module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
    (
    -- * Creating a request
      AdminUpdateAuthEventFeedback (..)
    , mkAdminUpdateAuthEventFeedback
    -- ** Request lenses
    , auaefUserPoolId
    , auaefUsername
    , auaefEventId
    , auaefFeedbackValue

    -- * Destructuring the response
    , AdminUpdateAuthEventFeedbackResponse (..)
    , mkAdminUpdateAuthEventFeedbackResponse
    -- ** Response lenses
    , auaefrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminUpdateAuthEventFeedback' smart constructor.
data AdminUpdateAuthEventFeedback = AdminUpdateAuthEventFeedback'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , username :: Types.Username
    -- ^ The user pool username.
  , eventId :: Types.EventId
    -- ^ The authentication event ID.
  , feedbackValue :: Types.FeedbackValueType
    -- ^ The authentication event feedback value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateAuthEventFeedback' value with any optional fields omitted.
mkAdminUpdateAuthEventFeedback
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> Types.EventId -- ^ 'eventId'
    -> Types.FeedbackValueType -- ^ 'feedbackValue'
    -> AdminUpdateAuthEventFeedback
mkAdminUpdateAuthEventFeedback userPoolId username eventId
  feedbackValue
  = AdminUpdateAuthEventFeedback'{userPoolId, username, eventId,
                                  feedbackValue}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefUserPoolId :: Lens.Lens' AdminUpdateAuthEventFeedback Types.UserPoolId
auaefUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE auaefUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user pool username.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefUsername :: Lens.Lens' AdminUpdateAuthEventFeedback Types.Username
auaefUsername = Lens.field @"username"
{-# INLINEABLE auaefUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The authentication event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefEventId :: Lens.Lens' AdminUpdateAuthEventFeedback Types.EventId
auaefEventId = Lens.field @"eventId"
{-# INLINEABLE auaefEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | The authentication event feedback value.
--
-- /Note:/ Consider using 'feedbackValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefFeedbackValue :: Lens.Lens' AdminUpdateAuthEventFeedback Types.FeedbackValueType
auaefFeedbackValue = Lens.field @"feedbackValue"
{-# INLINEABLE auaefFeedbackValue #-}
{-# DEPRECATED feedbackValue "Use generic-lens or generic-optics with 'feedbackValue' instead"  #-}

instance Core.ToQuery AdminUpdateAuthEventFeedback where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminUpdateAuthEventFeedback where
        toHeaders AdminUpdateAuthEventFeedback{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminUpdateAuthEventFeedback")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminUpdateAuthEventFeedback where
        toJSON AdminUpdateAuthEventFeedback{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("EventId" Core..= eventId),
                  Core.Just ("FeedbackValue" Core..= feedbackValue)])

instance Core.AWSRequest AdminUpdateAuthEventFeedback where
        type Rs AdminUpdateAuthEventFeedback =
             AdminUpdateAuthEventFeedbackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminUpdateAuthEventFeedbackResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminUpdateAuthEventFeedbackResponse' smart constructor.
newtype AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateAuthEventFeedbackResponse' value with any optional fields omitted.
mkAdminUpdateAuthEventFeedbackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminUpdateAuthEventFeedbackResponse
mkAdminUpdateAuthEventFeedbackResponse responseStatus
  = AdminUpdateAuthEventFeedbackResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefrrsResponseStatus :: Lens.Lens' AdminUpdateAuthEventFeedbackResponse Core.Int
auaefrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE auaefrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
