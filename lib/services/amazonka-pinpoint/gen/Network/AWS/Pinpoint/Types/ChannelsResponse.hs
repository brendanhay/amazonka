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
-- Module      : Network.AWS.Pinpoint.Types.ChannelsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the general settings and status of all
-- channels for an application, including channels that aren\'t enabled for
-- the application.
--
-- /See:/ 'newChannelsResponse' smart constructor.
data ChannelsResponse = ChannelsResponse'
  { -- | A map that contains a multipart response for each channel. For each item
    -- in this object, the ChannelType is the key and the Channel is the value.
    channels :: Prelude.HashMap Prelude.Text ChannelResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'channelsResponse_channels' - A map that contains a multipart response for each channel. For each item
-- in this object, the ChannelType is the key and the Channel is the value.
newChannelsResponse ::
  ChannelsResponse
newChannelsResponse =
  ChannelsResponse' {channels = Prelude.mempty}

-- | A map that contains a multipart response for each channel. For each item
-- in this object, the ChannelType is the key and the Channel is the value.
channelsResponse_channels :: Lens.Lens' ChannelsResponse (Prelude.HashMap Prelude.Text ChannelResponse)
channelsResponse_channels = Lens.lens (\ChannelsResponse' {channels} -> channels) (\s@ChannelsResponse' {} a -> s {channels = a} :: ChannelsResponse) Prelude.. Lens.coerced

instance Core.FromJSON ChannelsResponse where
  parseJSON =
    Core.withObject
      "ChannelsResponse"
      ( \x ->
          ChannelsResponse'
            Prelude.<$> (x Core..:? "Channels" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ChannelsResponse

instance Prelude.NFData ChannelsResponse
