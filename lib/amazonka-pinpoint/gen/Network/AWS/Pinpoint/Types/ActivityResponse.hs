-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ActivityResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ActivityResponse
  ( ActivityResponse (..),

    -- * Smart constructor
    mkActivityResponse,

    -- * Lenses
    aState,
    aStart,
    aTimezonesCompletedCount,
    aTimezonesTotalCount,
    aResult,
    aTreatmentId,
    aSuccessfulEndpointCount,
    aEnd,
    aTotalEndpointCount,
    aScheduledStart,
    aCampaignId,
    aId,
    aApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an activity that was performed by a campaign.
--
-- /See:/ 'mkActivityResponse' smart constructor.
data ActivityResponse = ActivityResponse'
  { state ::
      Lude.Maybe Lude.Text,
    start :: Lude.Maybe Lude.Text,
    timezonesCompletedCount :: Lude.Maybe Lude.Int,
    timezonesTotalCount :: Lude.Maybe Lude.Int,
    result :: Lude.Maybe Lude.Text,
    treatmentId :: Lude.Maybe Lude.Text,
    successfulEndpointCount :: Lude.Maybe Lude.Int,
    end :: Lude.Maybe Lude.Text,
    totalEndpointCount :: Lude.Maybe Lude.Int,
    scheduledStart :: Lude.Maybe Lude.Text,
    campaignId :: Lude.Text,
    id :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the campaign applies to.
-- * 'campaignId' - The unique identifier for the campaign that the activity applies to.
-- * 'end' - The actual time, in ISO 8601 format, when the activity was marked CANCELLED or COMPLETED.
-- * 'id' - The unique identifier for the activity.
-- * 'result' - Specifies whether the activity succeeded. Possible values are SUCCESS and FAIL.
-- * 'scheduledStart' - The scheduled start time, in ISO 8601 format, for the activity.
-- * 'start' - The actual start time, in ISO 8601 format, of the activity.
-- * 'state' - The current status of the activity. Possible values are: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
-- * 'successfulEndpointCount' - The total number of endpoints that the campaign successfully delivered messages to.
-- * 'timezonesCompletedCount' - The total number of time zones that were completed.
-- * 'timezonesTotalCount' - The total number of unique time zones that are in the segment for the campaign.
-- * 'totalEndpointCount' - The total number of endpoints that the campaign attempted to deliver messages to.
-- * 'treatmentId' - The unique identifier for the campaign treatment that the activity applies to. A treatment is a variation of a campaign that's used for A/B testing of a campaign.
mkActivityResponse ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  ActivityResponse
mkActivityResponse pCampaignId_ pId_ pApplicationId_ =
  ActivityResponse'
    { state = Lude.Nothing,
      start = Lude.Nothing,
      timezonesCompletedCount = Lude.Nothing,
      timezonesTotalCount = Lude.Nothing,
      result = Lude.Nothing,
      treatmentId = Lude.Nothing,
      successfulEndpointCount = Lude.Nothing,
      end = Lude.Nothing,
      totalEndpointCount = Lude.Nothing,
      scheduledStart = Lude.Nothing,
      campaignId = pCampaignId_,
      id = pId_,
      applicationId = pApplicationId_
    }

-- | The current status of the activity. Possible values are: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, and COMPLETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aState = Lens.lens (state :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ActivityResponse)
{-# DEPRECATED aState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The actual start time, in ISO 8601 format, of the activity.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStart :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aStart = Lens.lens (start :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: ActivityResponse)
{-# DEPRECATED aStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The total number of time zones that were completed.
--
-- /Note:/ Consider using 'timezonesCompletedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimezonesCompletedCount :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Int)
aTimezonesCompletedCount = Lens.lens (timezonesCompletedCount :: ActivityResponse -> Lude.Maybe Lude.Int) (\s a -> s {timezonesCompletedCount = a} :: ActivityResponse)
{-# DEPRECATED aTimezonesCompletedCount "Use generic-lens or generic-optics with 'timezonesCompletedCount' instead." #-}

-- | The total number of unique time zones that are in the segment for the campaign.
--
-- /Note:/ Consider using 'timezonesTotalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimezonesTotalCount :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Int)
aTimezonesTotalCount = Lens.lens (timezonesTotalCount :: ActivityResponse -> Lude.Maybe Lude.Int) (\s a -> s {timezonesTotalCount = a} :: ActivityResponse)
{-# DEPRECATED aTimezonesTotalCount "Use generic-lens or generic-optics with 'timezonesTotalCount' instead." #-}

-- | Specifies whether the activity succeeded. Possible values are SUCCESS and FAIL.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResult :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aResult = Lens.lens (result :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: ActivityResponse)
{-# DEPRECATED aResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The unique identifier for the campaign treatment that the activity applies to. A treatment is a variation of a campaign that's used for A/B testing of a campaign.
--
-- /Note:/ Consider using 'treatmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTreatmentId :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aTreatmentId = Lens.lens (treatmentId :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {treatmentId = a} :: ActivityResponse)
{-# DEPRECATED aTreatmentId "Use generic-lens or generic-optics with 'treatmentId' instead." #-}

-- | The total number of endpoints that the campaign successfully delivered messages to.
--
-- /Note:/ Consider using 'successfulEndpointCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSuccessfulEndpointCount :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Int)
aSuccessfulEndpointCount = Lens.lens (successfulEndpointCount :: ActivityResponse -> Lude.Maybe Lude.Int) (\s a -> s {successfulEndpointCount = a} :: ActivityResponse)
{-# DEPRECATED aSuccessfulEndpointCount "Use generic-lens or generic-optics with 'successfulEndpointCount' instead." #-}

-- | The actual time, in ISO 8601 format, when the activity was marked CANCELLED or COMPLETED.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnd :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aEnd = Lens.lens (end :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: ActivityResponse)
{-# DEPRECATED aEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | The total number of endpoints that the campaign attempted to deliver messages to.
--
-- /Note:/ Consider using 'totalEndpointCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTotalEndpointCount :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Int)
aTotalEndpointCount = Lens.lens (totalEndpointCount :: ActivityResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalEndpointCount = a} :: ActivityResponse)
{-# DEPRECATED aTotalEndpointCount "Use generic-lens or generic-optics with 'totalEndpointCount' instead." #-}

-- | The scheduled start time, in ISO 8601 format, for the activity.
--
-- /Note:/ Consider using 'scheduledStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aScheduledStart :: Lens.Lens' ActivityResponse (Lude.Maybe Lude.Text)
aScheduledStart = Lens.lens (scheduledStart :: ActivityResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledStart = a} :: ActivityResponse)
{-# DEPRECATED aScheduledStart "Use generic-lens or generic-optics with 'scheduledStart' instead." #-}

-- | The unique identifier for the campaign that the activity applies to.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCampaignId :: Lens.Lens' ActivityResponse Lude.Text
aCampaignId = Lens.lens (campaignId :: ActivityResponse -> Lude.Text) (\s a -> s {campaignId = a} :: ActivityResponse)
{-# DEPRECATED aCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the activity.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' ActivityResponse Lude.Text
aId = Lens.lens (id :: ActivityResponse -> Lude.Text) (\s a -> s {id = a} :: ActivityResponse)
{-# DEPRECATED aId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier for the application that the campaign applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApplicationId :: Lens.Lens' ActivityResponse Lude.Text
aApplicationId = Lens.lens (applicationId :: ActivityResponse -> Lude.Text) (\s a -> s {applicationId = a} :: ActivityResponse)
{-# DEPRECATED aApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON ActivityResponse where
  parseJSON =
    Lude.withObject
      "ActivityResponse"
      ( \x ->
          ActivityResponse'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Start")
            Lude.<*> (x Lude..:? "TimezonesCompletedCount")
            Lude.<*> (x Lude..:? "TimezonesTotalCount")
            Lude.<*> (x Lude..:? "Result")
            Lude.<*> (x Lude..:? "TreatmentId")
            Lude.<*> (x Lude..:? "SuccessfulEndpointCount")
            Lude.<*> (x Lude..:? "End")
            Lude.<*> (x Lude..:? "TotalEndpointCount")
            Lude.<*> (x Lude..:? "ScheduledStart")
            Lude.<*> (x Lude..: "CampaignId")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "ApplicationId")
      )
