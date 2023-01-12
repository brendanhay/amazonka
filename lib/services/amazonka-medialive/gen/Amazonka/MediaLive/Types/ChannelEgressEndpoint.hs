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
-- Module      : Amazonka.MediaLive.Types.ChannelEgressEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ChannelEgressEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'newChannelEgressEndpoint' smart constructor.
data ChannelEgressEndpoint = ChannelEgressEndpoint'
  { -- | Public IP of where a channel\'s output comes from
    sourceIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ChannelEgressEndpoint where
  parseJSON =
    Data.withObject
      "ChannelEgressEndpoint"
      ( \x ->
          ChannelEgressEndpoint'
            Prelude.<$> (x Data..:? "sourceIp")
      )

instance Prelude.Hashable ChannelEgressEndpoint where
  hashWithSalt _salt ChannelEgressEndpoint' {..} =
    _salt `Prelude.hashWithSalt` sourceIp

instance Prelude.NFData ChannelEgressEndpoint where
  rnf ChannelEgressEndpoint' {..} = Prelude.rnf sourceIp
