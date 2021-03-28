{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelActivityTaskDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RequestCancelActivityTaskDecisionAttributes
  ( RequestCancelActivityTaskDecisionAttributes (..)
  -- * Smart constructor
  , mkRequestCancelActivityTaskDecisionAttributes
  -- * Lenses
  , rcatdaActivityId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ActivityId as Types

-- | Provides the details of the @RequestCancelActivityTask@ decision.
--
-- __Access Control__ 
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
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
--
-- /See:/ 'mkRequestCancelActivityTaskDecisionAttributes' smart constructor.
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes'
  { activityId :: Types.ActivityId
    -- ^ The @activityId@ of the activity task to be canceled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCancelActivityTaskDecisionAttributes' value with any optional fields omitted.
mkRequestCancelActivityTaskDecisionAttributes
    :: Types.ActivityId -- ^ 'activityId'
    -> RequestCancelActivityTaskDecisionAttributes
mkRequestCancelActivityTaskDecisionAttributes activityId
  = RequestCancelActivityTaskDecisionAttributes'{activityId}

-- | The @activityId@ of the activity task to be canceled.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcatdaActivityId :: Lens.Lens' RequestCancelActivityTaskDecisionAttributes Types.ActivityId
rcatdaActivityId = Lens.field @"activityId"
{-# INLINEABLE rcatdaActivityId #-}
{-# DEPRECATED activityId "Use generic-lens or generic-optics with 'activityId' instead"  #-}

instance Core.FromJSON RequestCancelActivityTaskDecisionAttributes
         where
        toJSON RequestCancelActivityTaskDecisionAttributes{..}
          = Core.object
              (Core.catMaybes [Core.Just ("activityId" Core..= activityId)])
