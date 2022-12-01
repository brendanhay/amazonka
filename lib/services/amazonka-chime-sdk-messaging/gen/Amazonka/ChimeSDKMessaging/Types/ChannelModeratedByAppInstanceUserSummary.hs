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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelModeratedByAppInstanceUserSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelModeratedByAppInstanceUserSummary where

import Amazonka.ChimeSDKMessaging.Types.ChannelSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of a moderated channel.
--
-- /See:/ 'newChannelModeratedByAppInstanceUserSummary' smart constructor.
data ChannelModeratedByAppInstanceUserSummary = ChannelModeratedByAppInstanceUserSummary'
  { -- | Summary of the details of a @Channel@.
    channelSummary :: Prelude.Maybe ChannelSummary
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelModeratedByAppInstanceUserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelSummary', 'channelModeratedByAppInstanceUserSummary_channelSummary' - Summary of the details of a @Channel@.
newChannelModeratedByAppInstanceUserSummary ::
  ChannelModeratedByAppInstanceUserSummary
newChannelModeratedByAppInstanceUserSummary =
  ChannelModeratedByAppInstanceUserSummary'
    { channelSummary =
        Prelude.Nothing
    }

-- | Summary of the details of a @Channel@.
channelModeratedByAppInstanceUserSummary_channelSummary :: Lens.Lens' ChannelModeratedByAppInstanceUserSummary (Prelude.Maybe ChannelSummary)
channelModeratedByAppInstanceUserSummary_channelSummary = Lens.lens (\ChannelModeratedByAppInstanceUserSummary' {channelSummary} -> channelSummary) (\s@ChannelModeratedByAppInstanceUserSummary' {} a -> s {channelSummary = a} :: ChannelModeratedByAppInstanceUserSummary)

instance
  Core.FromJSON
    ChannelModeratedByAppInstanceUserSummary
  where
  parseJSON =
    Core.withObject
      "ChannelModeratedByAppInstanceUserSummary"
      ( \x ->
          ChannelModeratedByAppInstanceUserSummary'
            Prelude.<$> (x Core..:? "ChannelSummary")
      )

instance
  Prelude.Hashable
    ChannelModeratedByAppInstanceUserSummary
  where
  hashWithSalt
    _salt
    ChannelModeratedByAppInstanceUserSummary' {..} =
      _salt `Prelude.hashWithSalt` channelSummary

instance
  Prelude.NFData
    ChannelModeratedByAppInstanceUserSummary
  where
  rnf ChannelModeratedByAppInstanceUserSummary' {..} =
    Prelude.rnf channelSummary
