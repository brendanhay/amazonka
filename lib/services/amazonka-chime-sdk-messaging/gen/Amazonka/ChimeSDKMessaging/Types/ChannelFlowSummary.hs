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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelFlowSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelFlowSummary where

import Amazonka.ChimeSDKMessaging.Types.Processor
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of details of a channel flow.
--
-- /See:/ 'newChannelFlowSummary' smart constructor.
data ChannelFlowSummary = ChannelFlowSummary'
  { -- | The name of the channel flow.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Information about the processor Lambda functions.
    processors :: Prelude.Maybe (Prelude.NonEmpty Processor),
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelFlowSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'channelFlowSummary_name' - The name of the channel flow.
--
-- 'processors', 'channelFlowSummary_processors' - Information about the processor Lambda functions.
--
-- 'channelFlowArn', 'channelFlowSummary_channelFlowArn' - The ARN of the channel flow.
newChannelFlowSummary ::
  ChannelFlowSummary
newChannelFlowSummary =
  ChannelFlowSummary'
    { name = Prelude.Nothing,
      processors = Prelude.Nothing,
      channelFlowArn = Prelude.Nothing
    }

-- | The name of the channel flow.
channelFlowSummary_name :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe Prelude.Text)
channelFlowSummary_name = Lens.lens (\ChannelFlowSummary' {name} -> name) (\s@ChannelFlowSummary' {} a -> s {name = a} :: ChannelFlowSummary) Prelude.. Lens.mapping Core._Sensitive

-- | Information about the processor Lambda functions.
channelFlowSummary_processors :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe (Prelude.NonEmpty Processor))
channelFlowSummary_processors = Lens.lens (\ChannelFlowSummary' {processors} -> processors) (\s@ChannelFlowSummary' {} a -> s {processors = a} :: ChannelFlowSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the channel flow.
channelFlowSummary_channelFlowArn :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe Prelude.Text)
channelFlowSummary_channelFlowArn = Lens.lens (\ChannelFlowSummary' {channelFlowArn} -> channelFlowArn) (\s@ChannelFlowSummary' {} a -> s {channelFlowArn = a} :: ChannelFlowSummary)

instance Core.FromJSON ChannelFlowSummary where
  parseJSON =
    Core.withObject
      "ChannelFlowSummary"
      ( \x ->
          ChannelFlowSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Processors")
            Prelude.<*> (x Core..:? "ChannelFlowArn")
      )

instance Prelude.Hashable ChannelFlowSummary where
  hashWithSalt _salt ChannelFlowSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` processors
      `Prelude.hashWithSalt` channelFlowArn

instance Prelude.NFData ChannelFlowSummary where
  rnf ChannelFlowSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf processors
      `Prelude.seq` Prelude.rnf channelFlowArn
