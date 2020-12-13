{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.MarkerRecordedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.MarkerRecordedEventAttributes
  ( MarkerRecordedEventAttributes (..),

    -- * Smart constructor
    mkMarkerRecordedEventAttributes,

    -- * Lenses
    mreaMarkerName,
    mreaDetails,
    mreaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @MarkerRecorded@ event.
--
-- /See:/ 'mkMarkerRecordedEventAttributes' smart constructor.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes'
  { -- | The name of the marker.
    markerName :: Lude.Text,
    -- | The details of the marker.
    details :: Lude.Maybe Lude.Text,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MarkerRecordedEventAttributes' with the minimum fields required to make a request.
--
-- * 'markerName' - The name of the marker.
-- * 'details' - The details of the marker.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
mkMarkerRecordedEventAttributes ::
  -- | 'markerName'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  MarkerRecordedEventAttributes
mkMarkerRecordedEventAttributes
  pMarkerName_
  pDecisionTaskCompletedEventId_ =
    MarkerRecordedEventAttributes'
      { markerName = pMarkerName_,
        details = Lude.Nothing,
        decisionTaskCompletedEventId = pDecisionTaskCompletedEventId_
      }

-- | The name of the marker.
--
-- /Note:/ Consider using 'markerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaMarkerName :: Lens.Lens' MarkerRecordedEventAttributes Lude.Text
mreaMarkerName = Lens.lens (markerName :: MarkerRecordedEventAttributes -> Lude.Text) (\s a -> s {markerName = a} :: MarkerRecordedEventAttributes)
{-# DEPRECATED mreaMarkerName "Use generic-lens or generic-optics with 'markerName' instead." #-}

-- | The details of the marker.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaDetails :: Lens.Lens' MarkerRecordedEventAttributes (Lude.Maybe Lude.Text)
mreaDetails = Lens.lens (details :: MarkerRecordedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: MarkerRecordedEventAttributes)
{-# DEPRECATED mreaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarker@ decision that requested this marker. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mreaDecisionTaskCompletedEventId :: Lens.Lens' MarkerRecordedEventAttributes Lude.Integer
mreaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: MarkerRecordedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: MarkerRecordedEventAttributes)
{-# DEPRECATED mreaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON MarkerRecordedEventAttributes where
  parseJSON =
    Lude.withObject
      "MarkerRecordedEventAttributes"
      ( \x ->
          MarkerRecordedEventAttributes'
            Lude.<$> (x Lude..: "markerName")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
