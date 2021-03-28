{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the risk configuration.
module Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
    (
    -- * Creating a request
      DescribeRiskConfiguration (..)
    , mkDescribeRiskConfiguration
    -- ** Request lenses
    , drcUserPoolId
    , drcClientId

    -- * Destructuring the response
    , DescribeRiskConfigurationResponse (..)
    , mkDescribeRiskConfigurationResponse
    -- ** Response lenses
    , drcrrsRiskConfiguration
    , drcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRiskConfiguration' smart constructor.
data DescribeRiskConfiguration = DescribeRiskConfiguration'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , clientId :: Core.Maybe Types.ClientIdType
    -- ^ The app client ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRiskConfiguration' value with any optional fields omitted.
mkDescribeRiskConfiguration
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> DescribeRiskConfiguration
mkDescribeRiskConfiguration userPoolId
  = DescribeRiskConfiguration'{userPoolId, clientId = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcUserPoolId :: Lens.Lens' DescribeRiskConfiguration Types.UserPoolId
drcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE drcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcClientId :: Lens.Lens' DescribeRiskConfiguration (Core.Maybe Types.ClientIdType)
drcClientId = Lens.field @"clientId"
{-# INLINEABLE drcClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

instance Core.ToQuery DescribeRiskConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRiskConfiguration where
        toHeaders DescribeRiskConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeRiskConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRiskConfiguration where
        toJSON DescribeRiskConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  ("ClientId" Core..=) Core.<$> clientId])

instance Core.AWSRequest DescribeRiskConfiguration where
        type Rs DescribeRiskConfiguration =
             DescribeRiskConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRiskConfigurationResponse' Core.<$>
                   (x Core..: "RiskConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRiskConfigurationResponse' smart constructor.
data DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse'
  { riskConfiguration :: Types.RiskConfigurationType
    -- ^ The risk configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRiskConfigurationResponse' value with any optional fields omitted.
mkDescribeRiskConfigurationResponse
    :: Types.RiskConfigurationType -- ^ 'riskConfiguration'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeRiskConfigurationResponse
mkDescribeRiskConfigurationResponse riskConfiguration
  responseStatus
  = DescribeRiskConfigurationResponse'{riskConfiguration,
                                       responseStatus}

-- | The risk configuration.
--
-- /Note:/ Consider using 'riskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsRiskConfiguration :: Lens.Lens' DescribeRiskConfigurationResponse Types.RiskConfigurationType
drcrrsRiskConfiguration = Lens.field @"riskConfiguration"
{-# INLINEABLE drcrrsRiskConfiguration #-}
{-# DEPRECATED riskConfiguration "Use generic-lens or generic-optics with 'riskConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DescribeRiskConfigurationResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
