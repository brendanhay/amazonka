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
-- Module      : Amazonka.Chime.Types.ChannelModeratorSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelModeratorSummary where

import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of a @ChannelModerator@.
--
-- /See:/ 'newChannelModeratorSummary' smart constructor.
data ChannelModeratorSummary = ChannelModeratorSummary'
  { -- | The data for a moderator.
    moderator :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelModeratorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'moderator', 'channelModeratorSummary_moderator' - The data for a moderator.
newChannelModeratorSummary ::
  ChannelModeratorSummary
newChannelModeratorSummary =
  ChannelModeratorSummary'
    { moderator =
        Prelude.Nothing
    }

-- | The data for a moderator.
channelModeratorSummary_moderator :: Lens.Lens' ChannelModeratorSummary (Prelude.Maybe Identity)
channelModeratorSummary_moderator = Lens.lens (\ChannelModeratorSummary' {moderator} -> moderator) (\s@ChannelModeratorSummary' {} a -> s {moderator = a} :: ChannelModeratorSummary)

instance Data.FromJSON ChannelModeratorSummary where
  parseJSON =
    Data.withObject
      "ChannelModeratorSummary"
      ( \x ->
          ChannelModeratorSummary'
            Prelude.<$> (x Data..:? "Moderator")
      )

instance Prelude.Hashable ChannelModeratorSummary where
  hashWithSalt _salt ChannelModeratorSummary' {..} =
    _salt `Prelude.hashWithSalt` moderator

instance Prelude.NFData ChannelModeratorSummary where
  rnf ChannelModeratorSummary' {..} =
    Prelude.rnf moderator
