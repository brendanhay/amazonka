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
-- Module      : Amazonka.Chime.Types.TerminationHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.TerminationHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The termination health details, including the source IP address and
-- timestamp of the last successful SIP @OPTIONS@ message from your SIP
-- infrastructure.
--
-- /See:/ 'newTerminationHealth' smart constructor.
data TerminationHealth = TerminationHealth'
  { -- | The source IP address.
    source :: Prelude.Maybe Prelude.Text,
    -- | The timestamp, in ISO 8601 format.
    timestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminationHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'terminationHealth_source' - The source IP address.
--
-- 'timestamp', 'terminationHealth_timestamp' - The timestamp, in ISO 8601 format.
newTerminationHealth ::
  TerminationHealth
newTerminationHealth =
  TerminationHealth'
    { source = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The source IP address.
terminationHealth_source :: Lens.Lens' TerminationHealth (Prelude.Maybe Prelude.Text)
terminationHealth_source = Lens.lens (\TerminationHealth' {source} -> source) (\s@TerminationHealth' {} a -> s {source = a} :: TerminationHealth)

-- | The timestamp, in ISO 8601 format.
terminationHealth_timestamp :: Lens.Lens' TerminationHealth (Prelude.Maybe Prelude.UTCTime)
terminationHealth_timestamp = Lens.lens (\TerminationHealth' {timestamp} -> timestamp) (\s@TerminationHealth' {} a -> s {timestamp = a} :: TerminationHealth) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TerminationHealth where
  parseJSON =
    Data.withObject
      "TerminationHealth"
      ( \x ->
          TerminationHealth'
            Prelude.<$> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable TerminationHealth where
  hashWithSalt _salt TerminationHealth' {..} =
    _salt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TerminationHealth where
  rnf TerminationHealth' {..} =
    Prelude.rnf source
      `Prelude.seq` Prelude.rnf timestamp
