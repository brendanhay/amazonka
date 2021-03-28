{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.TestAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests if a specified principal is authorized to perform an AWS IoT action on a specified resource. Use this to test and debug the authorization behavior of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestAuthorization
    (
    -- * Creating a request
      TestAuthorization (..)
    , mkTestAuthorization
    -- ** Request lenses
    , taAuthInfos
    , taClientId
    , taCognitoIdentityPoolId
    , taPolicyNamesToAdd
    , taPolicyNamesToSkip
    , taPrincipal

    -- * Destructuring the response
    , TestAuthorizationResponse (..)
    , mkTestAuthorizationResponse
    -- ** Response lenses
    , tarrsAuthResults
    , tarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestAuthorization' smart constructor.
data TestAuthorization = TestAuthorization'
  { authInfos :: Core.NonEmpty Types.AuthInfo
    -- ^ A list of authorization info objects. Simulating authorization will create a response for each @authInfo@ object in the list.
  , clientId :: Core.Maybe Types.ClientId
    -- ^ The MQTT client ID.
  , cognitoIdentityPoolId :: Core.Maybe Types.CognitoIdentityPoolId
    -- ^ The Cognito identity pool ID.
  , policyNamesToAdd :: Core.Maybe [Types.PolicyName]
    -- ^ When testing custom authorization, the policies specified here are treated as if they are attached to the principal being authorized.
  , policyNamesToSkip :: Core.Maybe [Types.PolicyName]
    -- ^ When testing custom authorization, the policies specified here are treated as if they are not attached to the principal being authorized.
  , principal :: Core.Maybe Types.Principal
    -- ^ The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestAuthorization' value with any optional fields omitted.
mkTestAuthorization
    :: Core.NonEmpty Types.AuthInfo -- ^ 'authInfos'
    -> TestAuthorization
mkTestAuthorization authInfos
  = TestAuthorization'{authInfos, clientId = Core.Nothing,
                       cognitoIdentityPoolId = Core.Nothing,
                       policyNamesToAdd = Core.Nothing, policyNamesToSkip = Core.Nothing,
                       principal = Core.Nothing}

-- | A list of authorization info objects. Simulating authorization will create a response for each @authInfo@ object in the list.
--
-- /Note:/ Consider using 'authInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAuthInfos :: Lens.Lens' TestAuthorization (Core.NonEmpty Types.AuthInfo)
taAuthInfos = Lens.field @"authInfos"
{-# INLINEABLE taAuthInfos #-}
{-# DEPRECATED authInfos "Use generic-lens or generic-optics with 'authInfos' instead"  #-}

-- | The MQTT client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taClientId :: Lens.Lens' TestAuthorization (Core.Maybe Types.ClientId)
taClientId = Lens.field @"clientId"
{-# INLINEABLE taClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The Cognito identity pool ID.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taCognitoIdentityPoolId :: Lens.Lens' TestAuthorization (Core.Maybe Types.CognitoIdentityPoolId)
taCognitoIdentityPoolId = Lens.field @"cognitoIdentityPoolId"
{-# INLINEABLE taCognitoIdentityPoolId #-}
{-# DEPRECATED cognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead"  #-}

-- | When testing custom authorization, the policies specified here are treated as if they are attached to the principal being authorized.
--
-- /Note:/ Consider using 'policyNamesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPolicyNamesToAdd :: Lens.Lens' TestAuthorization (Core.Maybe [Types.PolicyName])
taPolicyNamesToAdd = Lens.field @"policyNamesToAdd"
{-# INLINEABLE taPolicyNamesToAdd #-}
{-# DEPRECATED policyNamesToAdd "Use generic-lens or generic-optics with 'policyNamesToAdd' instead"  #-}

-- | When testing custom authorization, the policies specified here are treated as if they are not attached to the principal being authorized.
--
-- /Note:/ Consider using 'policyNamesToSkip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPolicyNamesToSkip :: Lens.Lens' TestAuthorization (Core.Maybe [Types.PolicyName])
taPolicyNamesToSkip = Lens.field @"policyNamesToSkip"
{-# INLINEABLE taPolicyNamesToSkip #-}
{-# DEPRECATED policyNamesToSkip "Use generic-lens or generic-optics with 'policyNamesToSkip' instead"  #-}

-- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPrincipal :: Lens.Lens' TestAuthorization (Core.Maybe Types.Principal)
taPrincipal = Lens.field @"principal"
{-# INLINEABLE taPrincipal #-}
{-# DEPRECATED principal "Use generic-lens or generic-optics with 'principal' instead"  #-}

instance Core.ToQuery TestAuthorization where
        toQuery TestAuthorization{..}
          = Core.maybe Core.mempty (Core.toQueryPair "clientId") clientId

instance Core.ToHeaders TestAuthorization where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON TestAuthorization where
        toJSON TestAuthorization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("authInfos" Core..= authInfos),
                  ("cognitoIdentityPoolId" Core..=) Core.<$> cognitoIdentityPoolId,
                  ("policyNamesToAdd" Core..=) Core.<$> policyNamesToAdd,
                  ("policyNamesToSkip" Core..=) Core.<$> policyNamesToSkip,
                  ("principal" Core..=) Core.<$> principal])

instance Core.AWSRequest TestAuthorization where
        type Rs TestAuthorization = TestAuthorizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/test-authorization",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TestAuthorizationResponse' Core.<$>
                   (x Core..:? "authResults") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTestAuthorizationResponse' smart constructor.
data TestAuthorizationResponse = TestAuthorizationResponse'
  { authResults :: Core.Maybe [Types.AuthResult]
    -- ^ The authentication results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestAuthorizationResponse' value with any optional fields omitted.
mkTestAuthorizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestAuthorizationResponse
mkTestAuthorizationResponse responseStatus
  = TestAuthorizationResponse'{authResults = Core.Nothing,
                               responseStatus}

-- | The authentication results.
--
-- /Note:/ Consider using 'authResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarrsAuthResults :: Lens.Lens' TestAuthorizationResponse (Core.Maybe [Types.AuthResult])
tarrsAuthResults = Lens.field @"authResults"
{-# INLINEABLE tarrsAuthResults #-}
{-# DEPRECATED authResults "Use generic-lens or generic-optics with 'authResults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarrsResponseStatus :: Lens.Lens' TestAuthorizationResponse Core.Int
tarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
