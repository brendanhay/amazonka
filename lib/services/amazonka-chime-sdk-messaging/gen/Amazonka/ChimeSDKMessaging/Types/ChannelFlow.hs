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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelFlow where

import Amazonka.ChimeSDKMessaging.Types.Processor
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel flow.
--
-- /See:/ 'newChannelFlow' smart constructor.
data ChannelFlow = ChannelFlow'
  { -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the channel flow was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time at which a channel flow was updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the channel flow.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Information about the processor Lambda functions.
    processors :: Prelude.Maybe (Prelude.NonEmpty Processor)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelFlowArn', 'channelFlow_channelFlowArn' - The ARN of the channel flow.
--
-- 'createdTimestamp', 'channelFlow_createdTimestamp' - The time at which the channel flow was created.
--
-- 'lastUpdatedTimestamp', 'channelFlow_lastUpdatedTimestamp' - The time at which a channel flow was updated.
--
-- 'name', 'channelFlow_name' - The name of the channel flow.
--
-- 'processors', 'channelFlow_processors' - Information about the processor Lambda functions.
newChannelFlow ::
  ChannelFlow
newChannelFlow =
  ChannelFlow'
    { channelFlowArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      processors = Prelude.Nothing
    }

-- | The ARN of the channel flow.
channelFlow_channelFlowArn :: Lens.Lens' ChannelFlow (Prelude.Maybe Prelude.Text)
channelFlow_channelFlowArn = Lens.lens (\ChannelFlow' {channelFlowArn} -> channelFlowArn) (\s@ChannelFlow' {} a -> s {channelFlowArn = a} :: ChannelFlow)

-- | The time at which the channel flow was created.
channelFlow_createdTimestamp :: Lens.Lens' ChannelFlow (Prelude.Maybe Prelude.UTCTime)
channelFlow_createdTimestamp = Lens.lens (\ChannelFlow' {createdTimestamp} -> createdTimestamp) (\s@ChannelFlow' {} a -> s {createdTimestamp = a} :: ChannelFlow) Prelude.. Lens.mapping Data._Time

-- | The time at which a channel flow was updated.
channelFlow_lastUpdatedTimestamp :: Lens.Lens' ChannelFlow (Prelude.Maybe Prelude.UTCTime)
channelFlow_lastUpdatedTimestamp = Lens.lens (\ChannelFlow' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelFlow' {} a -> s {lastUpdatedTimestamp = a} :: ChannelFlow) Prelude.. Lens.mapping Data._Time

-- | The name of the channel flow.
channelFlow_name :: Lens.Lens' ChannelFlow (Prelude.Maybe Prelude.Text)
channelFlow_name = Lens.lens (\ChannelFlow' {name} -> name) (\s@ChannelFlow' {} a -> s {name = a} :: ChannelFlow) Prelude.. Lens.mapping Data._Sensitive

-- | Information about the processor Lambda functions.
channelFlow_processors :: Lens.Lens' ChannelFlow (Prelude.Maybe (Prelude.NonEmpty Processor))
channelFlow_processors = Lens.lens (\ChannelFlow' {processors} -> processors) (\s@ChannelFlow' {} a -> s {processors = a} :: ChannelFlow) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ChannelFlow where
  parseJSON =
    Data.withObject
      "ChannelFlow"
      ( \x ->
          ChannelFlow'
            Prelude.<$> (x Data..:? "ChannelFlowArn")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Processors")
      )

instance Prelude.Hashable ChannelFlow where
  hashWithSalt _salt ChannelFlow' {..} =
    _salt `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` processors

instance Prelude.NFData ChannelFlow where
  rnf ChannelFlow' {..} =
    Prelude.rnf channelFlowArn
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf processors
