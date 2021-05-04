{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.ChannelEgressEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelEgressEndpoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'newChannelEgressEndpoint' smart constructor.
data ChannelEgressEndpoint = ChannelEgressEndpoint'
  { -- | Public IP of where a channel\'s output comes from
    sourceIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChannelEgressEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIp', 'channelEgressEndpoint_sourceIp' - Public IP of where a channel\'s output comes from
newChannelEgressEndpoint ::
  ChannelEgressEndpoint
newChannelEgressEndpoint =
  ChannelEgressEndpoint' {sourceIp = Prelude.Nothing}

-- | Public IP of where a channel\'s output comes from
channelEgressEndpoint_sourceIp :: Lens.Lens' ChannelEgressEndpoint (Prelude.Maybe Prelude.Text)
channelEgressEndpoint_sourceIp = Lens.lens (\ChannelEgressEndpoint' {sourceIp} -> sourceIp) (\s@ChannelEgressEndpoint' {} a -> s {sourceIp = a} :: ChannelEgressEndpoint)

instance Prelude.FromJSON ChannelEgressEndpoint where
  parseJSON =
    Prelude.withObject
      "ChannelEgressEndpoint"
      ( \x ->
          ChannelEgressEndpoint'
            Prelude.<$> (x Prelude..:? "sourceIp")
      )

instance Prelude.Hashable ChannelEgressEndpoint

instance Prelude.NFData ChannelEgressEndpoint
