{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /activity type/ . After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.
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
module Network.AWS.SWF.DeprecateActivityType
    (
    -- * Creating a request
      DeprecateActivityType (..)
    , mkDeprecateActivityType
    -- ** Request lenses
    , datfDomain
    , datfActivityType

    -- * Destructuring the response
    , DeprecateActivityTypeResponse (..)
    , mkDeprecateActivityTypeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkDeprecateActivityType' smart constructor.
data DeprecateActivityType = DeprecateActivityType'
  { domain :: Types.DomainName
    -- ^ The name of the domain in which the activity type is registered.
  , activityType :: Types.ActivityType
    -- ^ The activity type to deprecate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateActivityType' value with any optional fields omitted.
mkDeprecateActivityType
    :: Types.DomainName -- ^ 'domain'
    -> Types.ActivityType -- ^ 'activityType'
    -> DeprecateActivityType
mkDeprecateActivityType domain activityType
  = DeprecateActivityType'{domain, activityType}

-- | The name of the domain in which the activity type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datfDomain :: Lens.Lens' DeprecateActivityType Types.DomainName
datfDomain = Lens.field @"domain"
{-# INLINEABLE datfDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The activity type to deprecate.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datfActivityType :: Lens.Lens' DeprecateActivityType Types.ActivityType
datfActivityType = Lens.field @"activityType"
{-# INLINEABLE datfActivityType #-}
{-# DEPRECATED activityType "Use generic-lens or generic-optics with 'activityType' instead"  #-}

instance Core.ToQuery DeprecateActivityType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeprecateActivityType where
        toHeaders DeprecateActivityType{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.DeprecateActivityType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DeprecateActivityType where
        toJSON DeprecateActivityType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("activityType" Core..= activityType)])

instance Core.AWSRequest DeprecateActivityType where
        type Rs DeprecateActivityType = DeprecateActivityTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeprecateActivityTypeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeprecateActivityTypeResponse' smart constructor.
data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateActivityTypeResponse' value with any optional fields omitted.
mkDeprecateActivityTypeResponse
    :: DeprecateActivityTypeResponse
mkDeprecateActivityTypeResponse = DeprecateActivityTypeResponse'
