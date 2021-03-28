{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified activity type. This includes configuration settings provided when the type was registered and other general information about the type.
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
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @activityType.name@ : String constraint. The key is @swf:activityType.name@ .
--
--
--     * @activityType.version@ : String constraint. The key is @swf:activityType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DescribeActivityType
    (
    -- * Creating a request
      DescribeActivityType (..)
    , mkDescribeActivityType
    -- ** Request lenses
    , datDomain
    , datActivityType

    -- * Destructuring the response
    , DescribeActivityTypeResponse (..)
    , mkDescribeActivityTypeResponse
    -- ** Response lenses
    , datrrsTypeInfo
    , datrrsConfiguration
    , datrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDescribeActivityType' smart constructor.
data DescribeActivityType = DescribeActivityType'
  { domain :: Types.DomainName
    -- ^ The name of the domain in which the activity type is registered.
  , activityType :: Types.ActivityType
    -- ^ The activity type to get information about. Activity types are identified by the @name@ and @version@ that were supplied when the activity was registered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeActivityType' value with any optional fields omitted.
mkDescribeActivityType
    :: Types.DomainName -- ^ 'domain'
    -> Types.ActivityType -- ^ 'activityType'
    -> DescribeActivityType
mkDescribeActivityType domain activityType
  = DescribeActivityType'{domain, activityType}

-- | The name of the domain in which the activity type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datDomain :: Lens.Lens' DescribeActivityType Types.DomainName
datDomain = Lens.field @"domain"
{-# INLINEABLE datDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The activity type to get information about. Activity types are identified by the @name@ and @version@ that were supplied when the activity was registered.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datActivityType :: Lens.Lens' DescribeActivityType Types.ActivityType
datActivityType = Lens.field @"activityType"
{-# INLINEABLE datActivityType #-}
{-# DEPRECATED activityType "Use generic-lens or generic-optics with 'activityType' instead"  #-}

instance Core.ToQuery DescribeActivityType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeActivityType where
        toHeaders DescribeActivityType{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.DescribeActivityType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DescribeActivityType where
        toJSON DescribeActivityType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("activityType" Core..= activityType)])

instance Core.AWSRequest DescribeActivityType where
        type Rs DescribeActivityType = DescribeActivityTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeActivityTypeResponse' Core.<$>
                   (x Core..: "typeInfo") Core.<*> x Core..: "configuration" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Detailed information about an activity type.
--
-- /See:/ 'mkDescribeActivityTypeResponse' smart constructor.
data DescribeActivityTypeResponse = DescribeActivityTypeResponse'
  { typeInfo :: Types.ActivityTypeInfo
    -- ^ General information about the activity type.
--
-- The status of activity type (returned in the ActivityTypeInfo structure) can be one of the following.
--
--     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running. 
--
--
--     * @DEPRECATED@ – The type was deprecated using 'DeprecateActivityType' , but is still in use. You should keep workers supporting this type running. You cannot create new tasks of this type. 
--
--
  , configuration :: Types.ActivityTypeConfiguration
    -- ^ The configuration settings registered with the activity type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeActivityTypeResponse' value with any optional fields omitted.
mkDescribeActivityTypeResponse
    :: Types.ActivityTypeInfo -- ^ 'typeInfo'
    -> Types.ActivityTypeConfiguration -- ^ 'configuration'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeActivityTypeResponse
mkDescribeActivityTypeResponse typeInfo configuration
  responseStatus
  = DescribeActivityTypeResponse'{typeInfo, configuration,
                                  responseStatus}

-- | General information about the activity type.
--
-- The status of activity type (returned in the ActivityTypeInfo structure) can be one of the following.
--
--     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running. 
--
--
--     * @DEPRECATED@ – The type was deprecated using 'DeprecateActivityType' , but is still in use. You should keep workers supporting this type running. You cannot create new tasks of this type. 
--
--
--
-- /Note:/ Consider using 'typeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsTypeInfo :: Lens.Lens' DescribeActivityTypeResponse Types.ActivityTypeInfo
datrrsTypeInfo = Lens.field @"typeInfo"
{-# INLINEABLE datrrsTypeInfo #-}
{-# DEPRECATED typeInfo "Use generic-lens or generic-optics with 'typeInfo' instead"  #-}

-- | The configuration settings registered with the activity type.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsConfiguration :: Lens.Lens' DescribeActivityTypeResponse Types.ActivityTypeConfiguration
datrrsConfiguration = Lens.field @"configuration"
{-# INLINEABLE datrrsConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsResponseStatus :: Lens.Lens' DescribeActivityTypeResponse Core.Int
datrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE datrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
