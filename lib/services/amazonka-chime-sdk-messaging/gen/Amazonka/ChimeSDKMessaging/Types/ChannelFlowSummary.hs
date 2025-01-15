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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelFlowSummary where

import Amazonka.ChimeSDKMessaging.Types.Processor
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of details of a channel flow.
--
-- /See:/ 'newChannelFlowSummary' smart constructor.
data ChannelFlowSummary = ChannelFlowSummary'
  { -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel flow.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Information about the processor Lambda functions.
    processors :: Prelude.Maybe (Prelude.NonEmpty Processor)
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
-- 'channelFlowArn', 'channelFlowSummary_channelFlowArn' - The ARN of the channel flow.
--
-- 'name', 'channelFlowSummary_name' - The name of the channel flow.
--
-- 'processors', 'channelFlowSummary_processors' - Information about the processor Lambda functions.
newChannelFlowSummary ::
  ChannelFlowSummary
newChannelFlowSummary =
  ChannelFlowSummary'
    { channelFlowArn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      processors = Prelude.Nothing
    }

-- | The ARN of the channel flow.
channelFlowSummary_channelFlowArn :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe Prelude.Text)
channelFlowSummary_channelFlowArn = Lens.lens (\ChannelFlowSummary' {channelFlowArn} -> channelFlowArn) (\s@ChannelFlowSummary' {} a -> s {channelFlowArn = a} :: ChannelFlowSummary)

-- | The name of the channel flow.
channelFlowSummary_name :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe Prelude.Text)
channelFlowSummary_name = Lens.lens (\ChannelFlowSummary' {name} -> name) (\s@ChannelFlowSummary' {} a -> s {name = a} :: ChannelFlowSummary) Prelude.. Lens.mapping Data._Sensitive

-- | Information about the processor Lambda functions.
channelFlowSummary_processors :: Lens.Lens' ChannelFlowSummary (Prelude.Maybe (Prelude.NonEmpty Processor))
channelFlowSummary_processors = Lens.lens (\ChannelFlowSummary' {processors} -> processors) (\s@ChannelFlowSummary' {} a -> s {processors = a} :: ChannelFlowSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ChannelFlowSummary where
  parseJSON =
    Data.withObject
      "ChannelFlowSummary"
      ( \x ->
          ChannelFlowSummary'
            Prelude.<$> (x Data..:? "ChannelFlowArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Processors")
      )

instance Prelude.Hashable ChannelFlowSummary where
  hashWithSalt _salt ChannelFlowSummary' {..} =
    _salt
      `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` processors

instance Prelude.NFData ChannelFlowSummary where
  rnf ChannelFlowSummary' {..} =
    Prelude.rnf channelFlowArn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf processors
