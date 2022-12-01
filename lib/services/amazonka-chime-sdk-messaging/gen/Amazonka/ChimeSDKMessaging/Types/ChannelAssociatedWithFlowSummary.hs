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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelAssociatedWithFlowSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelAssociatedWithFlowSummary where

import Amazonka.ChimeSDKMessaging.Types.ChannelMode
import Amazonka.ChimeSDKMessaging.Types.ChannelPrivacy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of details of a channel associated with channel flow.
--
-- /See:/ 'newChannelAssociatedWithFlowSummary' smart constructor.
data ChannelAssociatedWithFlowSummary = ChannelAssociatedWithFlowSummary'
  { -- | The name of the channel flow.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The channel\'s metadata.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The channel\'s privacy setting.
    privacy :: Prelude.Maybe ChannelPrivacy,
    -- | The mode of the channel.
    mode :: Prelude.Maybe ChannelMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelAssociatedWithFlowSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'channelAssociatedWithFlowSummary_name' - The name of the channel flow.
--
-- 'metadata', 'channelAssociatedWithFlowSummary_metadata' - The channel\'s metadata.
--
-- 'channelArn', 'channelAssociatedWithFlowSummary_channelArn' - The ARN of the channel.
--
-- 'privacy', 'channelAssociatedWithFlowSummary_privacy' - The channel\'s privacy setting.
--
-- 'mode', 'channelAssociatedWithFlowSummary_mode' - The mode of the channel.
newChannelAssociatedWithFlowSummary ::
  ChannelAssociatedWithFlowSummary
newChannelAssociatedWithFlowSummary =
  ChannelAssociatedWithFlowSummary'
    { name =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      privacy = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The name of the channel flow.
channelAssociatedWithFlowSummary_name :: Lens.Lens' ChannelAssociatedWithFlowSummary (Prelude.Maybe Prelude.Text)
channelAssociatedWithFlowSummary_name = Lens.lens (\ChannelAssociatedWithFlowSummary' {name} -> name) (\s@ChannelAssociatedWithFlowSummary' {} a -> s {name = a} :: ChannelAssociatedWithFlowSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The channel\'s metadata.
channelAssociatedWithFlowSummary_metadata :: Lens.Lens' ChannelAssociatedWithFlowSummary (Prelude.Maybe Prelude.Text)
channelAssociatedWithFlowSummary_metadata = Lens.lens (\ChannelAssociatedWithFlowSummary' {metadata} -> metadata) (\s@ChannelAssociatedWithFlowSummary' {} a -> s {metadata = a} :: ChannelAssociatedWithFlowSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The ARN of the channel.
channelAssociatedWithFlowSummary_channelArn :: Lens.Lens' ChannelAssociatedWithFlowSummary (Prelude.Maybe Prelude.Text)
channelAssociatedWithFlowSummary_channelArn = Lens.lens (\ChannelAssociatedWithFlowSummary' {channelArn} -> channelArn) (\s@ChannelAssociatedWithFlowSummary' {} a -> s {channelArn = a} :: ChannelAssociatedWithFlowSummary)

-- | The channel\'s privacy setting.
channelAssociatedWithFlowSummary_privacy :: Lens.Lens' ChannelAssociatedWithFlowSummary (Prelude.Maybe ChannelPrivacy)
channelAssociatedWithFlowSummary_privacy = Lens.lens (\ChannelAssociatedWithFlowSummary' {privacy} -> privacy) (\s@ChannelAssociatedWithFlowSummary' {} a -> s {privacy = a} :: ChannelAssociatedWithFlowSummary)

-- | The mode of the channel.
channelAssociatedWithFlowSummary_mode :: Lens.Lens' ChannelAssociatedWithFlowSummary (Prelude.Maybe ChannelMode)
channelAssociatedWithFlowSummary_mode = Lens.lens (\ChannelAssociatedWithFlowSummary' {mode} -> mode) (\s@ChannelAssociatedWithFlowSummary' {} a -> s {mode = a} :: ChannelAssociatedWithFlowSummary)

instance
  Core.FromJSON
    ChannelAssociatedWithFlowSummary
  where
  parseJSON =
    Core.withObject
      "ChannelAssociatedWithFlowSummary"
      ( \x ->
          ChannelAssociatedWithFlowSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "Privacy")
            Prelude.<*> (x Core..:? "Mode")
      )

instance
  Prelude.Hashable
    ChannelAssociatedWithFlowSummary
  where
  hashWithSalt
    _salt
    ChannelAssociatedWithFlowSummary' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` metadata
        `Prelude.hashWithSalt` channelArn
        `Prelude.hashWithSalt` privacy
        `Prelude.hashWithSalt` mode

instance
  Prelude.NFData
    ChannelAssociatedWithFlowSummary
  where
  rnf ChannelAssociatedWithFlowSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf privacy
      `Prelude.seq` Prelude.rnf mode
