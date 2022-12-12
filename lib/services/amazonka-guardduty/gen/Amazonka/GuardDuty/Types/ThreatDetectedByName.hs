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
-- Module      : Amazonka.GuardDuty.Types.ThreatDetectedByName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ThreatDetectedByName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ScanThreatName
import qualified Amazonka.Prelude as Prelude

-- | Contains details about identified threats organized by threat name.
--
-- /See:/ 'newThreatDetectedByName' smart constructor.
data ThreatDetectedByName = ThreatDetectedByName'
  { -- | Total number of infected files identified.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | Flag to determine if the finding contains every single infected
    -- file-path and\/or every threat.
    shortened :: Prelude.Maybe Prelude.Bool,
    -- | List of identified threats with details, organized by threat name.
    threatNames :: Prelude.Maybe [ScanThreatName],
    -- | Total number of unique threats by name identified, as part of the
    -- malware scan.
    uniqueThreatNameCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThreatDetectedByName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemCount', 'threatDetectedByName_itemCount' - Total number of infected files identified.
--
-- 'shortened', 'threatDetectedByName_shortened' - Flag to determine if the finding contains every single infected
-- file-path and\/or every threat.
--
-- 'threatNames', 'threatDetectedByName_threatNames' - List of identified threats with details, organized by threat name.
--
-- 'uniqueThreatNameCount', 'threatDetectedByName_uniqueThreatNameCount' - Total number of unique threats by name identified, as part of the
-- malware scan.
newThreatDetectedByName ::
  ThreatDetectedByName
newThreatDetectedByName =
  ThreatDetectedByName'
    { itemCount = Prelude.Nothing,
      shortened = Prelude.Nothing,
      threatNames = Prelude.Nothing,
      uniqueThreatNameCount = Prelude.Nothing
    }

-- | Total number of infected files identified.
threatDetectedByName_itemCount :: Lens.Lens' ThreatDetectedByName (Prelude.Maybe Prelude.Int)
threatDetectedByName_itemCount = Lens.lens (\ThreatDetectedByName' {itemCount} -> itemCount) (\s@ThreatDetectedByName' {} a -> s {itemCount = a} :: ThreatDetectedByName)

-- | Flag to determine if the finding contains every single infected
-- file-path and\/or every threat.
threatDetectedByName_shortened :: Lens.Lens' ThreatDetectedByName (Prelude.Maybe Prelude.Bool)
threatDetectedByName_shortened = Lens.lens (\ThreatDetectedByName' {shortened} -> shortened) (\s@ThreatDetectedByName' {} a -> s {shortened = a} :: ThreatDetectedByName)

-- | List of identified threats with details, organized by threat name.
threatDetectedByName_threatNames :: Lens.Lens' ThreatDetectedByName (Prelude.Maybe [ScanThreatName])
threatDetectedByName_threatNames = Lens.lens (\ThreatDetectedByName' {threatNames} -> threatNames) (\s@ThreatDetectedByName' {} a -> s {threatNames = a} :: ThreatDetectedByName) Prelude.. Lens.mapping Lens.coerced

-- | Total number of unique threats by name identified, as part of the
-- malware scan.
threatDetectedByName_uniqueThreatNameCount :: Lens.Lens' ThreatDetectedByName (Prelude.Maybe Prelude.Int)
threatDetectedByName_uniqueThreatNameCount = Lens.lens (\ThreatDetectedByName' {uniqueThreatNameCount} -> uniqueThreatNameCount) (\s@ThreatDetectedByName' {} a -> s {uniqueThreatNameCount = a} :: ThreatDetectedByName)

instance Data.FromJSON ThreatDetectedByName where
  parseJSON =
    Data.withObject
      "ThreatDetectedByName"
      ( \x ->
          ThreatDetectedByName'
            Prelude.<$> (x Data..:? "itemCount")
            Prelude.<*> (x Data..:? "shortened")
            Prelude.<*> (x Data..:? "threatNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "uniqueThreatNameCount")
      )

instance Prelude.Hashable ThreatDetectedByName where
  hashWithSalt _salt ThreatDetectedByName' {..} =
    _salt `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` shortened
      `Prelude.hashWithSalt` threatNames
      `Prelude.hashWithSalt` uniqueThreatNameCount

instance Prelude.NFData ThreatDetectedByName where
  rnf ThreatDetectedByName' {..} =
    Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf shortened
      `Prelude.seq` Prelude.rnf threatNames
      `Prelude.seq` Prelude.rnf uniqueThreatNameCount
