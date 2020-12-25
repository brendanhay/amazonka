{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RecordMarkerDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RecordMarkerDecisionAttributes
  ( RecordMarkerDecisionAttributes (..),

    -- * Smart constructor
    mkRecordMarkerDecisionAttributes,

    -- * Lenses
    rmdaMarkerName,
    rmdaDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.MarkerName as Types

-- | Provides the details of the @RecordMarker@ decision.
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
-- /See:/ 'mkRecordMarkerDecisionAttributes' smart constructor.
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes'
  { -- | The name of the marker.
    markerName :: Types.MarkerName,
    -- | The details of the marker.
    details :: Core.Maybe Types.Data
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordMarkerDecisionAttributes' value with any optional fields omitted.
mkRecordMarkerDecisionAttributes ::
  -- | 'markerName'
  Types.MarkerName ->
  RecordMarkerDecisionAttributes
mkRecordMarkerDecisionAttributes markerName =
  RecordMarkerDecisionAttributes'
    { markerName,
      details = Core.Nothing
    }

-- | The name of the marker.
--
-- /Note:/ Consider using 'markerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdaMarkerName :: Lens.Lens' RecordMarkerDecisionAttributes Types.MarkerName
rmdaMarkerName = Lens.field @"markerName"
{-# DEPRECATED rmdaMarkerName "Use generic-lens or generic-optics with 'markerName' instead." #-}

-- | The details of the marker.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdaDetails :: Lens.Lens' RecordMarkerDecisionAttributes (Core.Maybe Types.Data)
rmdaDetails = Lens.field @"details"
{-# DEPRECATED rmdaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Core.FromJSON RecordMarkerDecisionAttributes where
  toJSON RecordMarkerDecisionAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("markerName" Core..= markerName),
            ("details" Core..=) Core.<$> details
          ]
      )
