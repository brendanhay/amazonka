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
-- Module      : Amazonka.ConnectCampaigns.Types.Campaign
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.Campaign where

import Amazonka.ConnectCampaigns.Types.DialerConfig
import Amazonka.ConnectCampaigns.Types.OutboundCallConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Connect campaign.
--
-- /See:/ 'newCampaign' smart constructor.
data Campaign = Campaign'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    arn :: Prelude.Text,
    connectInstanceId :: Prelude.Text,
    dialerConfig :: DialerConfig,
    id :: Prelude.Text,
    name :: Prelude.Text,
    outboundCallConfig :: OutboundCallConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Campaign' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'campaign_tags' - Undocumented member.
--
-- 'arn', 'campaign_arn' - Undocumented member.
--
-- 'connectInstanceId', 'campaign_connectInstanceId' - Undocumented member.
--
-- 'dialerConfig', 'campaign_dialerConfig' - Undocumented member.
--
-- 'id', 'campaign_id' - Undocumented member.
--
-- 'name', 'campaign_name' - Undocumented member.
--
-- 'outboundCallConfig', 'campaign_outboundCallConfig' - Undocumented member.
newCampaign ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'connectInstanceId'
  Prelude.Text ->
  -- | 'dialerConfig'
  DialerConfig ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'outboundCallConfig'
  OutboundCallConfig ->
  Campaign
newCampaign
  pArn_
  pConnectInstanceId_
  pDialerConfig_
  pId_
  pName_
  pOutboundCallConfig_ =
    Campaign'
      { tags = Prelude.Nothing,
        arn = pArn_,
        connectInstanceId = pConnectInstanceId_,
        dialerConfig = pDialerConfig_,
        id = pId_,
        name = pName_,
        outboundCallConfig = pOutboundCallConfig_
      }

-- | Undocumented member.
campaign_tags :: Lens.Lens' Campaign (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
campaign_tags = Lens.lens (\Campaign' {tags} -> tags) (\s@Campaign' {} a -> s {tags = a} :: Campaign) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
campaign_arn :: Lens.Lens' Campaign Prelude.Text
campaign_arn = Lens.lens (\Campaign' {arn} -> arn) (\s@Campaign' {} a -> s {arn = a} :: Campaign)

-- | Undocumented member.
campaign_connectInstanceId :: Lens.Lens' Campaign Prelude.Text
campaign_connectInstanceId = Lens.lens (\Campaign' {connectInstanceId} -> connectInstanceId) (\s@Campaign' {} a -> s {connectInstanceId = a} :: Campaign)

-- | Undocumented member.
campaign_dialerConfig :: Lens.Lens' Campaign DialerConfig
campaign_dialerConfig = Lens.lens (\Campaign' {dialerConfig} -> dialerConfig) (\s@Campaign' {} a -> s {dialerConfig = a} :: Campaign)

-- | Undocumented member.
campaign_id :: Lens.Lens' Campaign Prelude.Text
campaign_id = Lens.lens (\Campaign' {id} -> id) (\s@Campaign' {} a -> s {id = a} :: Campaign)

-- | Undocumented member.
campaign_name :: Lens.Lens' Campaign Prelude.Text
campaign_name = Lens.lens (\Campaign' {name} -> name) (\s@Campaign' {} a -> s {name = a} :: Campaign)

-- | Undocumented member.
campaign_outboundCallConfig :: Lens.Lens' Campaign OutboundCallConfig
campaign_outboundCallConfig = Lens.lens (\Campaign' {outboundCallConfig} -> outboundCallConfig) (\s@Campaign' {} a -> s {outboundCallConfig = a} :: Campaign)

instance Data.FromJSON Campaign where
  parseJSON =
    Data.withObject
      "Campaign"
      ( \x ->
          Campaign'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "connectInstanceId")
            Prelude.<*> (x Data..: "dialerConfig")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "outboundCallConfig")
      )

instance Prelude.Hashable Campaign where
  hashWithSalt _salt Campaign' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` connectInstanceId
      `Prelude.hashWithSalt` dialerConfig
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outboundCallConfig

instance Prelude.NFData Campaign where
  rnf Campaign' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf connectInstanceId `Prelude.seq`
          Prelude.rnf dialerConfig `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf outboundCallConfig
