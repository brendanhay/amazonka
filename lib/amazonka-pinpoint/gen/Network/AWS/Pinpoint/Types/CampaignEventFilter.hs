-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEventFilter
  ( CampaignEventFilter (..),

    -- * Smart constructor
    mkCampaignEventFilter,

    -- * Lenses
    cefFilterType,
    cefDimensions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for events that cause a campaign to be sent.
--
-- /See:/ 'mkCampaignEventFilter' smart constructor.
data CampaignEventFilter = CampaignEventFilter'
  { filterType ::
      FilterType,
    dimensions :: EventDimensions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CampaignEventFilter' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimension settings of the event filter for the campaign.
-- * 'filterType' - The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
mkCampaignEventFilter ::
  -- | 'filterType'
  FilterType ->
  -- | 'dimensions'
  EventDimensions ->
  CampaignEventFilter
mkCampaignEventFilter pFilterType_ pDimensions_ =
  CampaignEventFilter'
    { filterType = pFilterType_,
      dimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent. Valid values are: SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT, sends the campaign when an endpoint event (<link>Events resource) occurs.
--
-- /Note:/ Consider using 'filterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefFilterType :: Lens.Lens' CampaignEventFilter FilterType
cefFilterType = Lens.lens (filterType :: CampaignEventFilter -> FilterType) (\s a -> s {filterType = a} :: CampaignEventFilter)
{-# DEPRECATED cefFilterType "Use generic-lens or generic-optics with 'filterType' instead." #-}

-- | The dimension settings of the event filter for the campaign.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cefDimensions :: Lens.Lens' CampaignEventFilter EventDimensions
cefDimensions = Lens.lens (dimensions :: CampaignEventFilter -> EventDimensions) (\s a -> s {dimensions = a} :: CampaignEventFilter)
{-# DEPRECATED cefDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON CampaignEventFilter where
  parseJSON =
    Lude.withObject
      "CampaignEventFilter"
      ( \x ->
          CampaignEventFilter'
            Lude.<$> (x Lude..: "FilterType") Lude.<*> (x Lude..: "Dimensions")
      )

instance Lude.ToJSON CampaignEventFilter where
  toJSON CampaignEventFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FilterType" Lude..= filterType),
            Lude.Just ("Dimensions" Lude..= dimensions)
          ]
      )
