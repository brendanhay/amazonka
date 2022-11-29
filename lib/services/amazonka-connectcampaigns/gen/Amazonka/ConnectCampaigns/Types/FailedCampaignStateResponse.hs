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
-- Module      : Amazonka.ConnectCampaigns.Types.FailedCampaignStateResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.FailedCampaignStateResponse where

import Amazonka.ConnectCampaigns.Types.GetCampaignStateBatchFailureCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Failed response of campaign state
--
-- /See:/ 'newFailedCampaignStateResponse' smart constructor.
data FailedCampaignStateResponse = FailedCampaignStateResponse'
  { failureCode :: Prelude.Maybe GetCampaignStateBatchFailureCode,
    campaignId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCampaignStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'failedCampaignStateResponse_failureCode' - Undocumented member.
--
-- 'campaignId', 'failedCampaignStateResponse_campaignId' - Undocumented member.
newFailedCampaignStateResponse ::
  FailedCampaignStateResponse
newFailedCampaignStateResponse =
  FailedCampaignStateResponse'
    { failureCode =
        Prelude.Nothing,
      campaignId = Prelude.Nothing
    }

-- | Undocumented member.
failedCampaignStateResponse_failureCode :: Lens.Lens' FailedCampaignStateResponse (Prelude.Maybe GetCampaignStateBatchFailureCode)
failedCampaignStateResponse_failureCode = Lens.lens (\FailedCampaignStateResponse' {failureCode} -> failureCode) (\s@FailedCampaignStateResponse' {} a -> s {failureCode = a} :: FailedCampaignStateResponse)

-- | Undocumented member.
failedCampaignStateResponse_campaignId :: Lens.Lens' FailedCampaignStateResponse (Prelude.Maybe Prelude.Text)
failedCampaignStateResponse_campaignId = Lens.lens (\FailedCampaignStateResponse' {campaignId} -> campaignId) (\s@FailedCampaignStateResponse' {} a -> s {campaignId = a} :: FailedCampaignStateResponse)

instance Core.FromJSON FailedCampaignStateResponse where
  parseJSON =
    Core.withObject
      "FailedCampaignStateResponse"
      ( \x ->
          FailedCampaignStateResponse'
            Prelude.<$> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "campaignId")
      )

instance Prelude.Hashable FailedCampaignStateResponse where
  hashWithSalt _salt FailedCampaignStateResponse' {..} =
    _salt `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` campaignId

instance Prelude.NFData FailedCampaignStateResponse where
  rnf FailedCampaignStateResponse' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf campaignId
