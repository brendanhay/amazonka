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
-- Module      : Amazonka.ConnectCampaigns.Types.SuccessfulCampaignStateResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.SuccessfulCampaignStateResponse where

import Amazonka.ConnectCampaigns.Types.CampaignState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Successful response of campaign state
--
-- /See:/ 'newSuccessfulCampaignStateResponse' smart constructor.
data SuccessfulCampaignStateResponse = SuccessfulCampaignStateResponse'
  { campaignId :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe CampaignState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessfulCampaignStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignId', 'successfulCampaignStateResponse_campaignId' - Undocumented member.
--
-- 'state', 'successfulCampaignStateResponse_state' - Undocumented member.
newSuccessfulCampaignStateResponse ::
  SuccessfulCampaignStateResponse
newSuccessfulCampaignStateResponse =
  SuccessfulCampaignStateResponse'
    { campaignId =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Undocumented member.
successfulCampaignStateResponse_campaignId :: Lens.Lens' SuccessfulCampaignStateResponse (Prelude.Maybe Prelude.Text)
successfulCampaignStateResponse_campaignId = Lens.lens (\SuccessfulCampaignStateResponse' {campaignId} -> campaignId) (\s@SuccessfulCampaignStateResponse' {} a -> s {campaignId = a} :: SuccessfulCampaignStateResponse)

-- | Undocumented member.
successfulCampaignStateResponse_state :: Lens.Lens' SuccessfulCampaignStateResponse (Prelude.Maybe CampaignState)
successfulCampaignStateResponse_state = Lens.lens (\SuccessfulCampaignStateResponse' {state} -> state) (\s@SuccessfulCampaignStateResponse' {} a -> s {state = a} :: SuccessfulCampaignStateResponse)

instance
  Data.FromJSON
    SuccessfulCampaignStateResponse
  where
  parseJSON =
    Data.withObject
      "SuccessfulCampaignStateResponse"
      ( \x ->
          SuccessfulCampaignStateResponse'
            Prelude.<$> (x Data..:? "campaignId")
            Prelude.<*> (x Data..:? "state")
      )

instance
  Prelude.Hashable
    SuccessfulCampaignStateResponse
  where
  hashWithSalt
    _salt
    SuccessfulCampaignStateResponse' {..} =
      _salt
        `Prelude.hashWithSalt` campaignId
        `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    SuccessfulCampaignStateResponse
  where
  rnf SuccessfulCampaignStateResponse' {..} =
    Prelude.rnf campaignId
      `Prelude.seq` Prelude.rnf state
