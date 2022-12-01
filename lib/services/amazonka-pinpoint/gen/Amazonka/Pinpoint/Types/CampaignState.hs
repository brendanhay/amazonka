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
-- Module      : Amazonka.Pinpoint.Types.CampaignState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types.CampaignStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status of a campaign.
--
-- /See:/ 'newCampaignState' smart constructor.
data CampaignState = CampaignState'
  { -- | The current status of the campaign, or the current status of a treatment
    -- that belongs to an A\/B test campaign.
    --
    -- If a campaign uses A\/B testing, the campaign has a status of COMPLETED
    -- only if all campaign treatments have a status of COMPLETED. If you
    -- delete the segment that\'s associated with a campaign, the campaign
    -- fails and has a status of DELETED.
    campaignStatus :: Prelude.Maybe CampaignStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignStatus', 'campaignState_campaignStatus' - The current status of the campaign, or the current status of a treatment
-- that belongs to an A\/B test campaign.
--
-- If a campaign uses A\/B testing, the campaign has a status of COMPLETED
-- only if all campaign treatments have a status of COMPLETED. If you
-- delete the segment that\'s associated with a campaign, the campaign
-- fails and has a status of DELETED.
newCampaignState ::
  CampaignState
newCampaignState =
  CampaignState' {campaignStatus = Prelude.Nothing}

-- | The current status of the campaign, or the current status of a treatment
-- that belongs to an A\/B test campaign.
--
-- If a campaign uses A\/B testing, the campaign has a status of COMPLETED
-- only if all campaign treatments have a status of COMPLETED. If you
-- delete the segment that\'s associated with a campaign, the campaign
-- fails and has a status of DELETED.
campaignState_campaignStatus :: Lens.Lens' CampaignState (Prelude.Maybe CampaignStatus)
campaignState_campaignStatus = Lens.lens (\CampaignState' {campaignStatus} -> campaignStatus) (\s@CampaignState' {} a -> s {campaignStatus = a} :: CampaignState)

instance Core.FromJSON CampaignState where
  parseJSON =
    Core.withObject
      "CampaignState"
      ( \x ->
          CampaignState'
            Prelude.<$> (x Core..:? "CampaignStatus")
      )

instance Prelude.Hashable CampaignState where
  hashWithSalt _salt CampaignState' {..} =
    _salt `Prelude.hashWithSalt` campaignStatus

instance Prelude.NFData CampaignState where
  rnf CampaignState' {..} = Prelude.rnf campaignStatus
