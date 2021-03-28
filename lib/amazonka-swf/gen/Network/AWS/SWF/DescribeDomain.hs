{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified domain, including description and status.
--
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DescribeDomain
    (
    -- * Creating a request
      DescribeDomain (..)
    , mkDescribeDomain
    -- ** Request lenses
    , ddName

    -- * Destructuring the response
    , DescribeDomainResponse (..)
    , mkDescribeDomainResponse
    -- ** Response lenses
    , ddrrsDomainInfo
    , ddrrsConfiguration
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDescribeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain'
  { name :: Types.DomainName
    -- ^ The name of the domain to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomain' value with any optional fields omitted.
mkDescribeDomain
    :: Types.DomainName -- ^ 'name'
    -> DescribeDomain
mkDescribeDomain name = DescribeDomain'{name}

-- | The name of the domain to describe.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DescribeDomain Types.DomainName
ddName = Lens.field @"name"
{-# INLINEABLE ddName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DescribeDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDomain where
        toHeaders DescribeDomain{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.DescribeDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeDomain where
        toJSON DescribeDomain{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.AWSRequest DescribeDomain where
        type Rs DescribeDomain = DescribeDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDomainResponse' Core.<$>
                   (x Core..: "domainInfo") Core.<*> x Core..: "configuration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains details of a domain.
--
-- /See:/ 'mkDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { domainInfo :: Types.DomainInfo
    -- ^ The basic information about a domain, such as its name, status, and description.
  , configuration :: Types.DomainConfiguration
    -- ^ The domain configuration. Currently, this includes only the domain's retention period.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomainResponse' value with any optional fields omitted.
mkDescribeDomainResponse
    :: Types.DomainInfo -- ^ 'domainInfo'
    -> Types.DomainConfiguration -- ^ 'configuration'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeDomainResponse
mkDescribeDomainResponse domainInfo configuration responseStatus
  = DescribeDomainResponse'{domainInfo, configuration,
                            responseStatus}

-- | The basic information about a domain, such as its name, status, and description.
--
-- /Note:/ Consider using 'domainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainInfo :: Lens.Lens' DescribeDomainResponse Types.DomainInfo
ddrrsDomainInfo = Lens.field @"domainInfo"
{-# INLINEABLE ddrrsDomainInfo #-}
{-# DEPRECATED domainInfo "Use generic-lens or generic-optics with 'domainInfo' instead"  #-}

-- | The domain configuration. Currently, this includes only the domain's retention period.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsConfiguration :: Lens.Lens' DescribeDomainResponse Types.DomainConfiguration
ddrrsConfiguration = Lens.field @"configuration"
{-# INLINEABLE ddrrsConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDomainResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
