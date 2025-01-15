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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.Channel where

import Amazonka.ChimeSDKMessaging.Types.ChannelMode
import Amazonka.ChimeSDKMessaging.Types.ChannelPrivacy
import Amazonka.ChimeSDKMessaging.Types.ElasticChannelConfiguration
import Amazonka.ChimeSDKMessaging.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The ARN of a channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel flow.
    channelFlowArn :: Prelude.Maybe Prelude.Text,
    -- | The @AppInstanceUser@ who created the channel.
    createdBy :: Prelude.Maybe Identity,
    -- | The time at which the @AppInstanceUser@ created the channel.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The attributes required to configure and create an elastic channel. An
    -- elastic channel can support a maximum of 1-million members.
    elasticChannelConfiguration :: Prelude.Maybe ElasticChannelConfiguration,
    -- | The time at which a member sent the last message in the channel.
    lastMessageTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time at which a channel was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The channel\'s metadata.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The mode of the channel.
    mode :: Prelude.Maybe ChannelMode,
    -- | The name of a channel.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The channel\'s privacy setting.
    privacy :: Prelude.Maybe ChannelPrivacy
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
-- 'channelArn', 'channel_channelArn' - The ARN of a channel.
--
-- 'channelFlowArn', 'channel_channelFlowArn' - The ARN of the channel flow.
--
-- 'createdBy', 'channel_createdBy' - The @AppInstanceUser@ who created the channel.
--
-- 'createdTimestamp', 'channel_createdTimestamp' - The time at which the @AppInstanceUser@ created the channel.
--
-- 'elasticChannelConfiguration', 'channel_elasticChannelConfiguration' - The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million members.
--
-- 'lastMessageTimestamp', 'channel_lastMessageTimestamp' - The time at which a member sent the last message in the channel.
--
-- 'lastUpdatedTimestamp', 'channel_lastUpdatedTimestamp' - The time at which a channel was last updated.
--
-- 'metadata', 'channel_metadata' - The channel\'s metadata.
--
-- 'mode', 'channel_mode' - The mode of the channel.
--
-- 'name', 'channel_name' - The name of a channel.
--
-- 'privacy', 'channel_privacy' - The channel\'s privacy setting.
newChannel ::
  Channel
newChannel =
  Channel'
    { channelArn = Prelude.Nothing,
      channelFlowArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      elasticChannelConfiguration = Prelude.Nothing,
      lastMessageTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      metadata = Prelude.Nothing,
      mode = Prelude.Nothing,
      name = Prelude.Nothing,
      privacy = Prelude.Nothing
    }

-- | The ARN of a channel.
channel_channelArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_channelArn = Lens.lens (\Channel' {channelArn} -> channelArn) (\s@Channel' {} a -> s {channelArn = a} :: Channel)

-- | The ARN of the channel flow.
channel_channelFlowArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_channelFlowArn = Lens.lens (\Channel' {channelFlowArn} -> channelFlowArn) (\s@Channel' {} a -> s {channelFlowArn = a} :: Channel)

-- | The @AppInstanceUser@ who created the channel.
channel_createdBy :: Lens.Lens' Channel (Prelude.Maybe Identity)
channel_createdBy = Lens.lens (\Channel' {createdBy} -> createdBy) (\s@Channel' {} a -> s {createdBy = a} :: Channel)

-- | The time at which the @AppInstanceUser@ created the channel.
channel_createdTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_createdTimestamp = Lens.lens (\Channel' {createdTimestamp} -> createdTimestamp) (\s@Channel' {} a -> s {createdTimestamp = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million members.
channel_elasticChannelConfiguration :: Lens.Lens' Channel (Prelude.Maybe ElasticChannelConfiguration)
channel_elasticChannelConfiguration = Lens.lens (\Channel' {elasticChannelConfiguration} -> elasticChannelConfiguration) (\s@Channel' {} a -> s {elasticChannelConfiguration = a} :: Channel)

-- | The time at which a member sent the last message in the channel.
channel_lastMessageTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastMessageTimestamp = Lens.lens (\Channel' {lastMessageTimestamp} -> lastMessageTimestamp) (\s@Channel' {} a -> s {lastMessageTimestamp = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The time at which a channel was last updated.
channel_lastUpdatedTimestamp :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastUpdatedTimestamp = Lens.lens (\Channel' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@Channel' {} a -> s {lastUpdatedTimestamp = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The channel\'s metadata.
channel_metadata :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_metadata = Lens.lens (\Channel' {metadata} -> metadata) (\s@Channel' {} a -> s {metadata = a} :: Channel) Prelude.. Lens.mapping Data._Sensitive

-- | The mode of the channel.
channel_mode :: Lens.Lens' Channel (Prelude.Maybe ChannelMode)
channel_mode = Lens.lens (\Channel' {mode} -> mode) (\s@Channel' {} a -> s {mode = a} :: Channel)

-- | The name of a channel.
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel) Prelude.. Lens.mapping Data._Sensitive

-- | The channel\'s privacy setting.
channel_privacy :: Lens.Lens' Channel (Prelude.Maybe ChannelPrivacy)
channel_privacy = Lens.lens (\Channel' {privacy} -> privacy) (\s@Channel' {} a -> s {privacy = a} :: Channel)

instance Data.FromJSON Channel where
  parseJSON =
    Data.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Data..:? "ChannelArn")
            Prelude.<*> (x Data..:? "ChannelFlowArn")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "ElasticChannelConfiguration")
            Prelude.<*> (x Data..:? "LastMessageTimestamp")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Privacy")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelFlowArn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` elasticChannelConfiguration
      `Prelude.hashWithSalt` lastMessageTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` privacy

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf channelFlowArn `Prelude.seq`
        Prelude.rnf createdBy `Prelude.seq`
          Prelude.rnf createdTimestamp `Prelude.seq`
            Prelude.rnf elasticChannelConfiguration `Prelude.seq`
              Prelude.rnf lastMessageTimestamp `Prelude.seq`
                Prelude.rnf lastUpdatedTimestamp `Prelude.seq`
                  Prelude.rnf metadata `Prelude.seq`
                    Prelude.rnf mode `Prelude.seq`
                      Prelude.rnf name `Prelude.seq`
                        Prelude.rnf privacy
