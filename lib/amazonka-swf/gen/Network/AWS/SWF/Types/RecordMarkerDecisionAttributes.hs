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
    rmdaDetails,
    rmdaMarkerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { details ::
      Lude.Maybe Lude.Text,
    markerName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordMarkerDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'details' - The details of the marker.
-- * 'markerName' - The name of the marker.
mkRecordMarkerDecisionAttributes ::
  -- | 'markerName'
  Lude.Text ->
  RecordMarkerDecisionAttributes
mkRecordMarkerDecisionAttributes pMarkerName_ =
  RecordMarkerDecisionAttributes'
    { details = Lude.Nothing,
      markerName = pMarkerName_
    }

-- | The details of the marker.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdaDetails :: Lens.Lens' RecordMarkerDecisionAttributes (Lude.Maybe Lude.Text)
rmdaDetails = Lens.lens (details :: RecordMarkerDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: RecordMarkerDecisionAttributes)
{-# DEPRECATED rmdaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The name of the marker.
--
-- /Note:/ Consider using 'markerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmdaMarkerName :: Lens.Lens' RecordMarkerDecisionAttributes Lude.Text
rmdaMarkerName = Lens.lens (markerName :: RecordMarkerDecisionAttributes -> Lude.Text) (\s a -> s {markerName = a} :: RecordMarkerDecisionAttributes)
{-# DEPRECATED rmdaMarkerName "Use generic-lens or generic-optics with 'markerName' instead." #-}

instance Lude.ToJSON RecordMarkerDecisionAttributes where
  toJSON RecordMarkerDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("details" Lude..=) Lude.<$> details,
            Lude.Just ("markerName" Lude..= markerName)
          ]
      )
