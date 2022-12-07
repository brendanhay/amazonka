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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.ChannelLatencyMode
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a channel.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Channel name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
    -- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
    -- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
    -- correspond to Ultra-low and Standard, respectively.)
    latencyMode :: Prelude.Maybe ChannelLatencyMode,
    -- | Channel ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Whether the channel is private (enabled for playback authorization).
    -- Default: @false@.
    authorized :: Prelude.Maybe Prelude.Bool,
    -- | Recording-configuration ARN. A value other than an empty string
    -- indicates that recording is enabled. Default: \"\" (empty string,
    -- recording is disabled).
    recordingConfigurationArn :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'channelSummary_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'name', 'channelSummary_name' - Channel name.
--
-- 'latencyMode', 'channelSummary_latencyMode' - Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
--
-- 'arn', 'channelSummary_arn' - Channel ARN.
--
-- 'authorized', 'channelSummary_authorized' - Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
--
-- 'recordingConfigurationArn', 'channelSummary_recordingConfigurationArn' - Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      latencyMode = Prelude.Nothing,
      arn = Prelude.Nothing,
      authorized = Prelude.Nothing,
      recordingConfigurationArn = Prelude.Nothing
    }

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | Channel name.
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | Channel latency mode. Use @NORMAL@ to broadcast and deliver live video
-- up to Full HD. Use @LOW@ for near-real-time interaction with viewers.
-- Default: @LOW@. (Note: In the Amazon IVS console, @LOW@ and @NORMAL@
-- correspond to Ultra-low and Standard, respectively.)
channelSummary_latencyMode :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelLatencyMode)
channelSummary_latencyMode = Lens.lens (\ChannelSummary' {latencyMode} -> latencyMode) (\s@ChannelSummary' {} a -> s {latencyMode = a} :: ChannelSummary)

-- | Channel ARN.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | Whether the channel is private (enabled for playback authorization).
-- Default: @false@.
channelSummary_authorized :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Bool)
channelSummary_authorized = Lens.lens (\ChannelSummary' {authorized} -> authorized) (\s@ChannelSummary' {} a -> s {authorized = a} :: ChannelSummary)

-- | Recording-configuration ARN. A value other than an empty string
-- indicates that recording is enabled. Default: \"\" (empty string,
-- recording is disabled).
channelSummary_recordingConfigurationArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_recordingConfigurationArn = Lens.lens (\ChannelSummary' {recordingConfigurationArn} -> recordingConfigurationArn) (\s@ChannelSummary' {} a -> s {recordingConfigurationArn = a} :: ChannelSummary)

instance Data.FromJSON ChannelSummary where
  parseJSON =
    Data.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "latencyMode")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authorized")
            Prelude.<*> (x Data..:? "recordingConfigurationArn")
      )

instance Prelude.Hashable ChannelSummary where
  hashWithSalt _salt ChannelSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` latencyMode
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authorized
      `Prelude.hashWithSalt` recordingConfigurationArn

instance Prelude.NFData ChannelSummary where
  rnf ChannelSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf latencyMode
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorized
      `Prelude.seq` Prelude.rnf recordingConfigurationArn
