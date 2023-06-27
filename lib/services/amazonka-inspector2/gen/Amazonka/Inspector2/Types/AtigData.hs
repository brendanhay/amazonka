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
-- Module      : Amazonka.Inspector2.Types.AtigData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AtigData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Web Services Threat Intel Group (ATIG) details for a specific
-- vulnerability.
--
-- /See:/ 'newAtigData' smart constructor.
data AtigData = AtigData'
  { -- | The date and time this vulnerability was first observed.
    firstSeen :: Prelude.Maybe Data.POSIX,
    -- | The date and time this vulnerability was last observed.
    lastSeen :: Prelude.Maybe Data.POSIX,
    -- | The commercial sectors this vulnerability targets.
    targets :: Prelude.Maybe [Prelude.Text],
    -- | The <https://attack.mitre.org/ MITRE ATT&CK> tactics, techniques, and
    -- procedures (TTPs) associated with vulnerability.
    ttps :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AtigData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstSeen', 'atigData_firstSeen' - The date and time this vulnerability was first observed.
--
-- 'lastSeen', 'atigData_lastSeen' - The date and time this vulnerability was last observed.
--
-- 'targets', 'atigData_targets' - The commercial sectors this vulnerability targets.
--
-- 'ttps', 'atigData_ttps' - The <https://attack.mitre.org/ MITRE ATT&CK> tactics, techniques, and
-- procedures (TTPs) associated with vulnerability.
newAtigData ::
  AtigData
newAtigData =
  AtigData'
    { firstSeen = Prelude.Nothing,
      lastSeen = Prelude.Nothing,
      targets = Prelude.Nothing,
      ttps = Prelude.Nothing
    }

-- | The date and time this vulnerability was first observed.
atigData_firstSeen :: Lens.Lens' AtigData (Prelude.Maybe Prelude.UTCTime)
atigData_firstSeen = Lens.lens (\AtigData' {firstSeen} -> firstSeen) (\s@AtigData' {} a -> s {firstSeen = a} :: AtigData) Prelude.. Lens.mapping Data._Time

-- | The date and time this vulnerability was last observed.
atigData_lastSeen :: Lens.Lens' AtigData (Prelude.Maybe Prelude.UTCTime)
atigData_lastSeen = Lens.lens (\AtigData' {lastSeen} -> lastSeen) (\s@AtigData' {} a -> s {lastSeen = a} :: AtigData) Prelude.. Lens.mapping Data._Time

-- | The commercial sectors this vulnerability targets.
atigData_targets :: Lens.Lens' AtigData (Prelude.Maybe [Prelude.Text])
atigData_targets = Lens.lens (\AtigData' {targets} -> targets) (\s@AtigData' {} a -> s {targets = a} :: AtigData) Prelude.. Lens.mapping Lens.coerced

-- | The <https://attack.mitre.org/ MITRE ATT&CK> tactics, techniques, and
-- procedures (TTPs) associated with vulnerability.
atigData_ttps :: Lens.Lens' AtigData (Prelude.Maybe [Prelude.Text])
atigData_ttps = Lens.lens (\AtigData' {ttps} -> ttps) (\s@AtigData' {} a -> s {ttps = a} :: AtigData) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AtigData where
  parseJSON =
    Data.withObject
      "AtigData"
      ( \x ->
          AtigData'
            Prelude.<$> (x Data..:? "firstSeen")
            Prelude.<*> (x Data..:? "lastSeen")
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ttps" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AtigData where
  hashWithSalt _salt AtigData' {..} =
    _salt
      `Prelude.hashWithSalt` firstSeen
      `Prelude.hashWithSalt` lastSeen
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` ttps

instance Prelude.NFData AtigData where
  rnf AtigData' {..} =
    Prelude.rnf firstSeen
      `Prelude.seq` Prelude.rnf lastSeen
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf ttps
