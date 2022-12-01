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
-- Module      : Amazonka.Chime.Types.ChannelMembershipSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelMembershipSummary where

import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of a @ChannelMembership@.
--
-- /See:/ 'newChannelMembershipSummary' smart constructor.
data ChannelMembershipSummary = ChannelMembershipSummary'
  { -- | A member\'s summary data.
    member :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMembershipSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'channelMembershipSummary_member' - A member\'s summary data.
newChannelMembershipSummary ::
  ChannelMembershipSummary
newChannelMembershipSummary =
  ChannelMembershipSummary' {member = Prelude.Nothing}

-- | A member\'s summary data.
channelMembershipSummary_member :: Lens.Lens' ChannelMembershipSummary (Prelude.Maybe Identity)
channelMembershipSummary_member = Lens.lens (\ChannelMembershipSummary' {member} -> member) (\s@ChannelMembershipSummary' {} a -> s {member = a} :: ChannelMembershipSummary)

instance Core.FromJSON ChannelMembershipSummary where
  parseJSON =
    Core.withObject
      "ChannelMembershipSummary"
      ( \x ->
          ChannelMembershipSummary'
            Prelude.<$> (x Core..:? "Member")
      )

instance Prelude.Hashable ChannelMembershipSummary where
  hashWithSalt _salt ChannelMembershipSummary' {..} =
    _salt `Prelude.hashWithSalt` member

instance Prelude.NFData ChannelMembershipSummary where
  rnf ChannelMembershipSummary' {..} =
    Prelude.rnf member
