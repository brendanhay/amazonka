{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetId
    (
    -- * Creating a request
      GetId (..)
    , mkGetId
    -- ** Request lenses
    , giIdentityPoolId
    , giAccountId
    , giLogins

    -- * Destructuring the response
    , GetIdResponse (..)
    , mkGetIdResponse
    -- ** Response lenses
    , girrsIdentityId
    , girrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the GetId action.
--
-- /See:/ 'mkGetId' smart constructor.
data GetId = GetId'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  , accountId :: Core.Maybe Types.AccountId
    -- ^ A standard AWS account ID (9+ digits).
  , logins :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
    -- ^ A set of optional name-value pairs that map provider names to provider tokens. The available provider names for @Logins@ are as follows:
--
--
--     * Facebook: @graph.facebook.com@ 
--
--
--     * Amazon Cognito user pool: @cognito-idp.<region>.amazonaws.com/<YOUR_USER_POOL_ID>@ , for example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ . 
--
--
--     * Google: @accounts.google.com@ 
--
--
--     * Amazon: @www.amazon.com@ 
--
--
--     * Twitter: @api.twitter.com@ 
--
--
--     * Digits: @www.digits.com@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetId' value with any optional fields omitted.
mkGetId
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> GetId
mkGetId identityPoolId
  = GetId'{identityPoolId, accountId = Core.Nothing,
           logins = Core.Nothing}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giIdentityPoolId :: Lens.Lens' GetId Types.IdentityPoolId
giIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE giIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | A standard AWS account ID (9+ digits).
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAccountId :: Lens.Lens' GetId (Core.Maybe Types.AccountId)
giAccountId = Lens.field @"accountId"
{-# INLINEABLE giAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. The available provider names for @Logins@ are as follows:
--
--
--     * Facebook: @graph.facebook.com@ 
--
--
--     * Amazon Cognito user pool: @cognito-idp.<region>.amazonaws.com/<YOUR_USER_POOL_ID>@ , for example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ . 
--
--
--     * Google: @accounts.google.com@ 
--
--
--     * Amazon: @www.amazon.com@ 
--
--
--     * Twitter: @api.twitter.com@ 
--
--
--     * Digits: @www.digits.com@ 
--
--
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLogins :: Lens.Lens' GetId (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken))
giLogins = Lens.field @"logins"
{-# INLINEABLE giLogins #-}
{-# DEPRECATED logins "Use generic-lens or generic-optics with 'logins' instead"  #-}

instance Core.ToQuery GetId where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetId where
        toHeaders GetId{..}
          = Core.pure ("X-Amz-Target", "AWSCognitoIdentityService.GetId")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetId where
        toJSON GetId{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolId" Core..= identityPoolId),
                  ("AccountId" Core..=) Core.<$> accountId,
                  ("Logins" Core..=) Core.<$> logins])

instance Core.AWSRequest GetId where
        type Rs GetId = GetIdResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIdResponse' Core.<$>
                   (x Core..:? "IdentityId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a GetId request.
--
-- /See:/ 'mkGetIdResponse' smart constructor.
data GetIdResponse = GetIdResponse'
  { identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdResponse' value with any optional fields omitted.
mkGetIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIdResponse
mkGetIdResponse responseStatus
  = GetIdResponse'{identityId = Core.Nothing, responseStatus}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsIdentityId :: Lens.Lens' GetIdResponse (Core.Maybe Types.IdentityId)
girrsIdentityId = Lens.field @"identityId"
{-# INLINEABLE girrsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetIdResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE girrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
