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
-- Module      : Amazonka.IoT1ClickProjects.Types.PlacementSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.PlacementSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object providing summary information for a particular placement.
--
-- /See:/ 'newPlacementSummary' smart constructor.
data PlacementSummary = PlacementSummary'
  { -- | The name of the project containing the placement.
    projectName :: Prelude.Text,
    -- | The name of the placement being summarized.
    placementName :: Prelude.Text,
    -- | The date when the placement was originally created, in UNIX epoch time
    -- format.
    createdDate :: Data.POSIX,
    -- | The date when the placement was last updated, in UNIX epoch time format.
    -- If the placement was not updated, then @createdDate@ and @updatedDate@
    -- are the same.
    updatedDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'placementSummary_projectName' - The name of the project containing the placement.
--
-- 'placementName', 'placementSummary_placementName' - The name of the placement being summarized.
--
-- 'createdDate', 'placementSummary_createdDate' - The date when the placement was originally created, in UNIX epoch time
-- format.
--
-- 'updatedDate', 'placementSummary_updatedDate' - The date when the placement was last updated, in UNIX epoch time format.
-- If the placement was not updated, then @createdDate@ and @updatedDate@
-- are the same.
newPlacementSummary ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'placementName'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'updatedDate'
  Prelude.UTCTime ->
  PlacementSummary
newPlacementSummary
  pProjectName_
  pPlacementName_
  pCreatedDate_
  pUpdatedDate_ =
    PlacementSummary'
      { projectName = pProjectName_,
        placementName = pPlacementName_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        updatedDate = Data._Time Lens.# pUpdatedDate_
      }

-- | The name of the project containing the placement.
placementSummary_projectName :: Lens.Lens' PlacementSummary Prelude.Text
placementSummary_projectName = Lens.lens (\PlacementSummary' {projectName} -> projectName) (\s@PlacementSummary' {} a -> s {projectName = a} :: PlacementSummary)

-- | The name of the placement being summarized.
placementSummary_placementName :: Lens.Lens' PlacementSummary Prelude.Text
placementSummary_placementName = Lens.lens (\PlacementSummary' {placementName} -> placementName) (\s@PlacementSummary' {} a -> s {placementName = a} :: PlacementSummary)

-- | The date when the placement was originally created, in UNIX epoch time
-- format.
placementSummary_createdDate :: Lens.Lens' PlacementSummary Prelude.UTCTime
placementSummary_createdDate = Lens.lens (\PlacementSummary' {createdDate} -> createdDate) (\s@PlacementSummary' {} a -> s {createdDate = a} :: PlacementSummary) Prelude.. Data._Time

-- | The date when the placement was last updated, in UNIX epoch time format.
-- If the placement was not updated, then @createdDate@ and @updatedDate@
-- are the same.
placementSummary_updatedDate :: Lens.Lens' PlacementSummary Prelude.UTCTime
placementSummary_updatedDate = Lens.lens (\PlacementSummary' {updatedDate} -> updatedDate) (\s@PlacementSummary' {} a -> s {updatedDate = a} :: PlacementSummary) Prelude.. Data._Time

instance Data.FromJSON PlacementSummary where
  parseJSON =
    Data.withObject
      "PlacementSummary"
      ( \x ->
          PlacementSummary'
            Prelude.<$> (x Data..: "projectName")
            Prelude.<*> (x Data..: "placementName")
            Prelude.<*> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "updatedDate")
      )

instance Prelude.Hashable PlacementSummary where
  hashWithSalt _salt PlacementSummary' {..} =
    _salt `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` placementName
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` updatedDate

instance Prelude.NFData PlacementSummary where
  rnf PlacementSummary' {..} =
    Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf placementName
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf updatedDate
