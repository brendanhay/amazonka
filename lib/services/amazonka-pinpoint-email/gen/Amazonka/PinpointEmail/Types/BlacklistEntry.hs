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
-- Module      : Amazonka.PinpointEmail.Types.BlacklistEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.BlacklistEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a blacklisting event that
-- impacts one of the dedicated IP addresses that is associated with your
-- account.
--
-- /See:/ 'newBlacklistEntry' smart constructor.
data BlacklistEntry = BlacklistEntry'
  { -- | The time when the blacklisting event occurred, shown in Unix time
    -- format.
    listingTime :: Prelude.Maybe Data.POSIX,
    -- | Additional information about the blacklisting event, as provided by the
    -- blacklist maintainer.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the blacklist that the IP address appears on.
    rblName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlacklistEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listingTime', 'blacklistEntry_listingTime' - The time when the blacklisting event occurred, shown in Unix time
-- format.
--
-- 'description', 'blacklistEntry_description' - Additional information about the blacklisting event, as provided by the
-- blacklist maintainer.
--
-- 'rblName', 'blacklistEntry_rblName' - The name of the blacklist that the IP address appears on.
newBlacklistEntry ::
  BlacklistEntry
newBlacklistEntry =
  BlacklistEntry'
    { listingTime = Prelude.Nothing,
      description = Prelude.Nothing,
      rblName = Prelude.Nothing
    }

-- | The time when the blacklisting event occurred, shown in Unix time
-- format.
blacklistEntry_listingTime :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.UTCTime)
blacklistEntry_listingTime = Lens.lens (\BlacklistEntry' {listingTime} -> listingTime) (\s@BlacklistEntry' {} a -> s {listingTime = a} :: BlacklistEntry) Prelude.. Lens.mapping Data._Time

-- | Additional information about the blacklisting event, as provided by the
-- blacklist maintainer.
blacklistEntry_description :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.Text)
blacklistEntry_description = Lens.lens (\BlacklistEntry' {description} -> description) (\s@BlacklistEntry' {} a -> s {description = a} :: BlacklistEntry)

-- | The name of the blacklist that the IP address appears on.
blacklistEntry_rblName :: Lens.Lens' BlacklistEntry (Prelude.Maybe Prelude.Text)
blacklistEntry_rblName = Lens.lens (\BlacklistEntry' {rblName} -> rblName) (\s@BlacklistEntry' {} a -> s {rblName = a} :: BlacklistEntry)

instance Data.FromJSON BlacklistEntry where
  parseJSON =
    Data.withObject
      "BlacklistEntry"
      ( \x ->
          BlacklistEntry'
            Prelude.<$> (x Data..:? "ListingTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "RblName")
      )

instance Prelude.Hashable BlacklistEntry where
  hashWithSalt _salt BlacklistEntry' {..} =
    _salt `Prelude.hashWithSalt` listingTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` rblName

instance Prelude.NFData BlacklistEntry where
  rnf BlacklistEntry' {..} =
    Prelude.rnf listingTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf rblName
