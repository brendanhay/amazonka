{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignEventFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignEventFilter where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for events that cause a campaign to be sent.
--
-- /See:/ 'newCampaignEventFilter' smart constructor.
data CampaignEventFilter = CampaignEventFilter'
  { -- | The type of event that causes the campaign to be sent. Valid values are:
    -- SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT,
    -- sends the campaign when an endpoint event (Events resource) occurs.
    filterType :: FilterType,
    -- | The dimension settings of the event filter for the campaign.
    dimensions :: EventDimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CampaignEventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterType', 'campaignEventFilter_filterType' - The type of event that causes the campaign to be sent. Valid values are:
-- SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT,
-- sends the campaign when an endpoint event (Events resource) occurs.
--
-- 'dimensions', 'campaignEventFilter_dimensions' - The dimension settings of the event filter for the campaign.
newCampaignEventFilter ::
  -- | 'filterType'
  FilterType ->
  -- | 'dimensions'
  EventDimensions ->
  CampaignEventFilter
newCampaignEventFilter pFilterType_ pDimensions_ =
  CampaignEventFilter'
    { filterType = pFilterType_,
      dimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent. Valid values are:
-- SYSTEM, sends the campaign when a system event occurs; and, ENDPOINT,
-- sends the campaign when an endpoint event (Events resource) occurs.
campaignEventFilter_filterType :: Lens.Lens' CampaignEventFilter FilterType
campaignEventFilter_filterType = Lens.lens (\CampaignEventFilter' {filterType} -> filterType) (\s@CampaignEventFilter' {} a -> s {filterType = a} :: CampaignEventFilter)

-- | The dimension settings of the event filter for the campaign.
campaignEventFilter_dimensions :: Lens.Lens' CampaignEventFilter EventDimensions
campaignEventFilter_dimensions = Lens.lens (\CampaignEventFilter' {dimensions} -> dimensions) (\s@CampaignEventFilter' {} a -> s {dimensions = a} :: CampaignEventFilter)

instance Prelude.FromJSON CampaignEventFilter where
  parseJSON =
    Prelude.withObject
      "CampaignEventFilter"
      ( \x ->
          CampaignEventFilter'
            Prelude.<$> (x Prelude..: "FilterType")
            Prelude.<*> (x Prelude..: "Dimensions")
      )

instance Prelude.Hashable CampaignEventFilter

instance Prelude.NFData CampaignEventFilter

instance Prelude.ToJSON CampaignEventFilter where
  toJSON CampaignEventFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FilterType" Prelude..= filterType),
            Prelude.Just ("Dimensions" Prelude..= dimensions)
          ]
      )
