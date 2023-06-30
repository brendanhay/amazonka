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
-- Module      : Amazonka.ConnectCampaigns.Types.CampaignSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.CampaignSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Connect campaign summary.
--
-- /See:/ 'newCampaignSummary' smart constructor.
data CampaignSummary = CampaignSummary'
  { arn :: Prelude.Text,
    connectInstanceId :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CampaignSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'campaignSummary_arn' - Undocumented member.
--
-- 'connectInstanceId', 'campaignSummary_connectInstanceId' - Undocumented member.
--
-- 'id', 'campaignSummary_id' - Undocumented member.
--
-- 'name', 'campaignSummary_name' - Undocumented member.
newCampaignSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CampaignSummary
newCampaignSummary
  pArn_
  pConnectInstanceId_
  pId_
  pName_ =
    CampaignSummary'
      { arn = pArn_,
        connectInstanceId = pConnectInstanceId_,
        id = pId_,
        name = pName_
      }

-- | Undocumented member.
campaignSummary_arn :: Lens.Lens' CampaignSummary Prelude.Text
campaignSummary_arn = Lens.lens (\CampaignSummary' {arn} -> arn) (\s@CampaignSummary' {} a -> s {arn = a} :: CampaignSummary)

-- | Undocumented member.
campaignSummary_connectInstanceId :: Lens.Lens' CampaignSummary Prelude.Text
campaignSummary_connectInstanceId = Lens.lens (\CampaignSummary' {connectInstanceId} -> connectInstanceId) (\s@CampaignSummary' {} a -> s {connectInstanceId = a} :: CampaignSummary)

-- | Undocumented member.
campaignSummary_id :: Lens.Lens' CampaignSummary Prelude.Text
campaignSummary_id = Lens.lens (\CampaignSummary' {id} -> id) (\s@CampaignSummary' {} a -> s {id = a} :: CampaignSummary)

-- | Undocumented member.
campaignSummary_name :: Lens.Lens' CampaignSummary Prelude.Text
campaignSummary_name = Lens.lens (\CampaignSummary' {name} -> name) (\s@CampaignSummary' {} a -> s {name = a} :: CampaignSummary)

instance Data.FromJSON CampaignSummary where
  parseJSON =
    Data.withObject
      "CampaignSummary"
      ( \x ->
          CampaignSummary'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "connectInstanceId")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable CampaignSummary where
  hashWithSalt _salt CampaignSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData CampaignSummary where
  rnf CampaignSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf connectInstanceId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
