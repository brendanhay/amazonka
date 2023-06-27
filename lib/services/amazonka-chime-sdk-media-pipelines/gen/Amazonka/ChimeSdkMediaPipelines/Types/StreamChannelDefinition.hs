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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.StreamChannelDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.StreamChannelDefinition where

import Amazonka.ChimeSdkMediaPipelines.Types.ChannelDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a streaming channel.
--
-- /See:/ 'newStreamChannelDefinition' smart constructor.
data StreamChannelDefinition = StreamChannelDefinition'
  { -- | The definitions of the channels in a streaming channel.
    channelDefinitions :: Prelude.Maybe (Prelude.NonEmpty ChannelDefinition),
    -- | The number of channels in a streaming channel.
    numberOfChannels :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamChannelDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelDefinitions', 'streamChannelDefinition_channelDefinitions' - The definitions of the channels in a streaming channel.
--
-- 'numberOfChannels', 'streamChannelDefinition_numberOfChannels' - The number of channels in a streaming channel.
newStreamChannelDefinition ::
  -- | 'numberOfChannels'
  Prelude.Natural ->
  StreamChannelDefinition
newStreamChannelDefinition pNumberOfChannels_ =
  StreamChannelDefinition'
    { channelDefinitions =
        Prelude.Nothing,
      numberOfChannels = pNumberOfChannels_
    }

-- | The definitions of the channels in a streaming channel.
streamChannelDefinition_channelDefinitions :: Lens.Lens' StreamChannelDefinition (Prelude.Maybe (Prelude.NonEmpty ChannelDefinition))
streamChannelDefinition_channelDefinitions = Lens.lens (\StreamChannelDefinition' {channelDefinitions} -> channelDefinitions) (\s@StreamChannelDefinition' {} a -> s {channelDefinitions = a} :: StreamChannelDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The number of channels in a streaming channel.
streamChannelDefinition_numberOfChannels :: Lens.Lens' StreamChannelDefinition Prelude.Natural
streamChannelDefinition_numberOfChannels = Lens.lens (\StreamChannelDefinition' {numberOfChannels} -> numberOfChannels) (\s@StreamChannelDefinition' {} a -> s {numberOfChannels = a} :: StreamChannelDefinition)

instance Data.FromJSON StreamChannelDefinition where
  parseJSON =
    Data.withObject
      "StreamChannelDefinition"
      ( \x ->
          StreamChannelDefinition'
            Prelude.<$> (x Data..:? "ChannelDefinitions")
            Prelude.<*> (x Data..: "NumberOfChannels")
      )

instance Prelude.Hashable StreamChannelDefinition where
  hashWithSalt _salt StreamChannelDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` channelDefinitions
      `Prelude.hashWithSalt` numberOfChannels

instance Prelude.NFData StreamChannelDefinition where
  rnf StreamChannelDefinition' {..} =
    Prelude.rnf channelDefinitions
      `Prelude.seq` Prelude.rnf numberOfChannels

instance Data.ToJSON StreamChannelDefinition where
  toJSON StreamChannelDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelDefinitions" Data..=)
              Prelude.<$> channelDefinitions,
            Prelude.Just
              ("NumberOfChannels" Data..= numberOfChannels)
          ]
      )
