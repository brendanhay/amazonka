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
-- Module      : Amazonka.Pinpoint.Types.CampaignEventFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignEventFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EventDimensions
import Amazonka.Pinpoint.Types.FilterType
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON CampaignEventFilter where
  parseJSON =
    Data.withObject
      "CampaignEventFilter"
      ( \x ->
          CampaignEventFilter'
            Prelude.<$> (x Data..: "FilterType")
            Prelude.<*> (x Data..: "Dimensions")
      )

instance Prelude.Hashable CampaignEventFilter where
  hashWithSalt _salt CampaignEventFilter' {..} =
    _salt
      `Prelude.hashWithSalt` filterType
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData CampaignEventFilter where
  rnf CampaignEventFilter' {..} =
    Prelude.rnf filterType
      `Prelude.seq` Prelude.rnf dimensions

instance Data.ToJSON CampaignEventFilter where
  toJSON CampaignEventFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FilterType" Data..= filterType),
            Prelude.Just ("Dimensions" Data..= dimensions)
          ]
      )
