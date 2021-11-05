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
-- Module      : Amazonka.IVS.Types.ChannelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import Amazonka.IVS.Types.ChannelLatencyMode
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a channel.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Channel ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
    -- correspond to Ultra-low and Standard, respectively.)
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Recording-configuration ARN. A value other than an empty string
    -- indicates that recording is enabled. Default: \"\" (empty string,
    -- recording is disabled).
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorized', 'channelSummary_authorized' - Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
--
-- 'arn', 'channelSummary_arn' - Channel ARN.
--
-- 'latencyMode', 'channelSummary_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
--
-- 'name', 'channelSummary_name' - Channel name.
--
-- 'recordingConfigurationArn', 'channelSummary_recordingConfigurationArn' - Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
--
-- 'tags', 'channelSummary_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { authorized = Prelude.Nothing,
      arn = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      name = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
channelSummary_authorized :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Bool)
channelSummary_authorized = Lens.lens (\ChannelSummary' {authorized} -> authorized) (\s@ChannelSummary' {} a -> s {authorized = a} :: ChannelSummary)

-- | Channel ARN.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
channelSummary_latencyMode :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelLatencyMode)
channelSummary_latencyMode = Lens.lens (\ChannelSummary' {latencyMode} -> latencyMode) (\s@ChannelSummary' {} a -> s {latencyMode = a} :: ChannelSummary)

-- | Channel name.
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
channelSummary_recordingConfigurationArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_recordingConfigurationArn = Lens.lens (\ChannelSummary' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@ChannelSummary' {} a -> s {recordingConfigurationArn = a} :: ChannelSummary)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ChannelSummary where
  parseJSON =
    Core.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Core..:? "authorized")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "latencyMode")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "recordingConfigurationArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ChannelSummary

instance Prelude.NFData ChannelSummary
