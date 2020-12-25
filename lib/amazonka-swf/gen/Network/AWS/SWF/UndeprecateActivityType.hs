{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /activity type/ . After an activity type has been undeprecated, you can create new tasks of that activity type.
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
module Network.AWS.SWF.UndeprecateActivityType
  ( -- * Creating a request
    UndeprecateActivityType (..),
    mkUndeprecateActivityType,

    -- ** Request lenses
    uatDomain,
    uatActivityType,

    -- * Destructuring the response
    UndeprecateActivityTypeResponse (..),
    mkUndeprecateActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkUndeprecateActivityType' smart constructor.
data UndeprecateActivityType = UndeprecateActivityType'
  { -- | The name of the domain of the deprecated activity type.
    domain :: Types.DomainName,
    -- | The activity type to undeprecate.
    activityType :: Types.ActivityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateActivityType' value with any optional fields omitted.
mkUndeprecateActivityType ::
  -- | 'domain'
  Types.DomainName ->
  -- | 'activityType'
  Types.ActivityType ->
  UndeprecateActivityType
mkUndeprecateActivityType domain activityType =
  UndeprecateActivityType' {domain, activityType}

-- | The name of the domain of the deprecated activity type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatDomain :: Lens.Lens' UndeprecateActivityType Types.DomainName
uatDomain = Lens.field @"domain"
{-# DEPRECATED uatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The activity type to undeprecate.
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatActivityType :: Lens.Lens' UndeprecateActivityType Types.ActivityType
uatActivityType = Lens.field @"activityType"
{-# DEPRECATED uatActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

instance Core.FromJSON UndeprecateActivityType where
  toJSON UndeprecateActivityType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("activityType" Core..= activityType)
          ]
      )

instance Core.AWSRequest UndeprecateActivityType where
  type Rs UndeprecateActivityType = UndeprecateActivityTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.UndeprecateActivityType")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UndeprecateActivityTypeResponse'

-- | /See:/ 'mkUndeprecateActivityTypeResponse' smart constructor.
data UndeprecateActivityTypeResponse = UndeprecateActivityTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UndeprecateActivityTypeResponse' value with any optional fields omitted.
mkUndeprecateActivityTypeResponse ::
  UndeprecateActivityTypeResponse
mkUndeprecateActivityTypeResponse =
  UndeprecateActivityTypeResponse'
