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
-- Module      : Amazonka.ChimeSDKMessaging.Types.Channel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.Channel where

import Amazonka.ChimeSDKMessaging.Types.ChannelMode
import Amazonka.ChimeSDKMessaging.Types.ChannelPrivacy
import Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration
import Amazonka.ChimeSDKMessaging.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The time at which a channel was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The time at which a member sent the last message in the channel.
    lastMessageTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of a channel.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The attributes required to configure and create an elastic channel. An
    -- elastic channel can support a maximum of 1-million members.
    elasticChannelConfiguration :: Prelude.Maybe ElasticChannelConfiguration,
    -- | The channel\'s metadata.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time at which the @AppInstanceUser@ created the channel.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN of a channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The channel\'s privacy setting.
    privacy :: Prelude.Maybe ChannelPrivacy,
    -- | The mode of the channel.
    mode :: Prelude.Maybe ChannelMode,
    -- | The @AppInstanceUser@ who created the channel.
    createdBy :: Prelude.Maybe Identity,
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'channel_lastUpdatedTimestamp' - The time at which a channel was last updated.
--
-- 'lastMessageTimestamp', 'channel_lastMessageTimestamp' - The time at which a member sent the last message in the channel.
--
-- 'name', 'channel_name' - The name of a channel.
--
-- 'elasticChannelConfiguration', 'channel_elasticChannelConfiguration' - The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million members.
--
-- 'metadata', 'channel_metadata' - The channel\'s metadata.
--
-- 'createdTimestamp', 'channel_createdTimestamp' - The time at which the @AppInstanceUser@ created the channel.
--
-- 'channelArn', 'channel_channelArn' - The ARN of a channel.
--
-- 'privacy', 'channel_privacy' - The channel\'s privacy setting.
--
-- 'mode', 'channel_mode' - The mode of the channel.
--
-- 'createdBy', 'channel_createdBy' - The @AppInstanceUser@ who created the channel.
--
-- 'channelFlowArn', 'channel_channelFlowArn' - The ARN of the channel flow.
newChannel ::
  Channel
newChannel =
  Channel'
    { lastUpdatedTimestamp = Prelude.Nothing,
      lastMessageTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      elasticChannelConfiguration = Prelude.Nothing,
      metadata = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      privacy = Prelude.Nothing,
      mode = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      channelFlowArn = Prelude.Nothing
    }

-- | The time at which a channel was last updated.
channel_lastUpdatedTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastUpdatedTimestamp = Lens.lens (\Channel' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Channel' {} a -> s {lastUpdatedTimestamp = a} :: Channel) Prelude.. Lens.mapping Core._Time

-- | The time at which a member sent the last message in the channel.
channel_lastMessageTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastMessageTimestamp = Lens.lens (\Channel' {lastMessageTimestamp} -> lastMessageTimestamp) (\s@Channel' {} a -> s {lastMessageTimestamp = a} :: Channel) Prelude.. Lens.mapping Core._Time

-- | The name of a channel.
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel) Prelude.. Lens.mapping Core._Sensitive

-- | The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million members.
channel_elasticChannelConfiguration :: Lens.Lens' Channel (Prelude.Maybe ElasticChannelConfiguration)
channel_elasticChannelConfiguration = Lens.lens (\Channel' {elasticChannelConfiguration} -> elasticChannelConfiguration) (\s@Channel' {} a -> s {elasticChannelConfiguration = a} :: Channel)

-- | The channel\'s metadata.
channel_metadata :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_metadata = Lens.lens (\Channel' {metadata} -> metadata) (\s@Channel' {} a -> s {metadata = a} :: Channel) Prelude.. Lens.mapping Core._Sensitive

-- | The time at which the @AppInstanceUser@ created the channel.
channel_createdTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_createdTimestamp = Lens.lens (\Channel' {createdTimestamp} -> createdTimestamp) (\s@Channel' {} a -> s {createdTimestamp = a} :: Channel) Prelude.. Lens.mapping Core._Time

-- | The ARN of a channel.
channel_channelArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_channelArn = Lens.lens (\Channel' {channelArn} -> channelArn) (\s@Channel' {} a -> s {channelArn = a} :: Channel)

-- | The channel\'s privacy setting.
channel_privacy :: Lens.Lens' Channel (Prelude.Maybe ChannelPrivacy)
channel_privacy = Lens.lens (\Channel' {privacy} -> privacy) (\s@Channel' {} a -> s {privacy = a} :: Channel)

-- | The mode of the channel.
channel_mode :: Lens.Lens' Channel (Prelude.Maybe ChannelMode)
channel_mode = Lens.lens (\Channel' {mode} -> mode) (\s@Channel' {} a -> s {mode = a} :: Channel)

-- | The @AppInstanceUser@ who created the channel.
channel_createdBy :: Lens.Lens' Channel (Prelude.Maybe Identity)
channel_createdBy = Lens.lens (\Channel' {createdBy} -> createdBy) (\s@Channel' {} a -> s {createdBy = a} :: Channel)

-- | The ARN of the channel flow.
channel_channelFlowArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_channelFlowArn = Lens.lens (\Channel' {channelFlowArn} -> channelFlowArn) (\s@Channel' {} a -> s {channelFlowArn = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "LastMessageTimestamp")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ElasticChannelConfiguration")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "Privacy")
            Prelude.<*> (x Core..:? "Mode")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "ChannelFlowArn")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` lastMessageTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` elasticChannelConfiguration
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` privacy
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` channelFlowArn

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf lastMessageTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf elasticChannelConfiguration
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf privacy
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf channelFlowArn
