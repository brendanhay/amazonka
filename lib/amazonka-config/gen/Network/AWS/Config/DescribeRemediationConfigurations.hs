{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRemediationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation configurations.
module Network.AWS.Config.DescribeRemediationConfigurations
    (
    -- * Creating a request
      DescribeRemediationConfigurations (..)
    , mkDescribeRemediationConfigurations
    -- ** Request lenses
    , drcConfigRuleNames

    -- * Destructuring the response
    , DescribeRemediationConfigurationsResponse (..)
    , mkDescribeRemediationConfigurationsResponse
    -- ** Response lenses
    , drcrfrsRemediationConfigurations
    , drcrfrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRemediationConfigurations' smart constructor.
newtype DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { configRuleNames :: [Types.ConfigRuleName]
    -- ^ A list of AWS Config rule names of remediation configurations for which you want details. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRemediationConfigurations' value with any optional fields omitted.
mkDescribeRemediationConfigurations
    :: DescribeRemediationConfigurations
mkDescribeRemediationConfigurations
  = DescribeRemediationConfigurations'{configRuleNames = Core.mempty}

-- | A list of AWS Config rule names of remediation configurations for which you want details. 
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcConfigRuleNames :: Lens.Lens' DescribeRemediationConfigurations [Types.ConfigRuleName]
drcConfigRuleNames = Lens.field @"configRuleNames"
{-# INLINEABLE drcConfigRuleNames #-}
{-# DEPRECATED configRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead"  #-}

instance Core.ToQuery DescribeRemediationConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRemediationConfigurations where
        toHeaders DescribeRemediationConfigurations{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeRemediationConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRemediationConfigurations where
        toJSON DescribeRemediationConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleNames" Core..= configRuleNames)])

instance Core.AWSRequest DescribeRemediationConfigurations where
        type Rs DescribeRemediationConfigurations =
             DescribeRemediationConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRemediationConfigurationsResponse' Core.<$>
                   (x Core..:? "RemediationConfigurations") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRemediationConfigurationsResponse' smart constructor.
data DescribeRemediationConfigurationsResponse = DescribeRemediationConfigurationsResponse'
  { remediationConfigurations :: Core.Maybe [Types.RemediationConfiguration]
    -- ^ Returns a remediation configuration object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRemediationConfigurationsResponse' value with any optional fields omitted.
mkDescribeRemediationConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRemediationConfigurationsResponse
mkDescribeRemediationConfigurationsResponse responseStatus
  = DescribeRemediationConfigurationsResponse'{remediationConfigurations
                                                 = Core.Nothing,
                                               responseStatus}

-- | Returns a remediation configuration object.
--
-- /Note:/ Consider using 'remediationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrfrsRemediationConfigurations :: Lens.Lens' DescribeRemediationConfigurationsResponse (Core.Maybe [Types.RemediationConfiguration])
drcrfrsRemediationConfigurations = Lens.field @"remediationConfigurations"
{-# INLINEABLE drcrfrsRemediationConfigurations #-}
{-# DEPRECATED remediationConfigurations "Use generic-lens or generic-optics with 'remediationConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrfrsResponseStatus :: Lens.Lens' DescribeRemediationConfigurationsResponse Core.Int
drcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
