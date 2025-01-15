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
-- Module      : Amazonka.IoT1ClickProjects.Types.PlacementDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types.PlacementDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object describing a project\'s placement.
--
-- /See:/ 'newPlacementDescription' smart constructor.
data PlacementDescription = PlacementDescription'
  { -- | The name of the project containing the placement.
    projectName :: Prelude.Text,
    -- | The name of the placement.
    placementName :: Prelude.Text,
    -- | The user-defined attributes associated with the placement.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The date when the placement was initially created, in UNIX epoch time
    -- format.
    createdDate :: Data.POSIX,
    -- | The date when the placement was last updated, in UNIX epoch time format.
    -- If the placement was not updated, then @createdDate@ and @updatedDate@
    -- are the same.
    updatedDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacementDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'placementDescription_projectName' - The name of the project containing the placement.
--
-- 'placementName', 'placementDescription_placementName' - The name of the placement.
--
-- 'attributes', 'placementDescription_attributes' - The user-defined attributes associated with the placement.
--
-- 'createdDate', 'placementDescription_createdDate' - The date when the placement was initially created, in UNIX epoch time
-- format.
--
-- 'updatedDate', 'placementDescription_updatedDate' - The date when the placement was last updated, in UNIX epoch time format.
-- If the placement was not updated, then @createdDate@ and @updatedDate@
-- are the same.
newPlacementDescription ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'placementName'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'updatedDate'
  Prelude.UTCTime ->
  PlacementDescription
newPlacementDescription
  pProjectName_
  pPlacementName_
  pCreatedDate_
  pUpdatedDate_ =
    PlacementDescription'
      { projectName = pProjectName_,
        placementName = pPlacementName_,
        attributes = Prelude.mempty,
        createdDate = Data._Time Lens.# pCreatedDate_,
        updatedDate = Data._Time Lens.# pUpdatedDate_
      }

-- | The name of the project containing the placement.
placementDescription_projectName :: Lens.Lens' PlacementDescription Prelude.Text
placementDescription_projectName = Lens.lens (\PlacementDescription' {projectName} -> projectName) (\s@PlacementDescription' {} a -> s {projectName = a} :: PlacementDescription)

-- | The name of the placement.
placementDescription_placementName :: Lens.Lens' PlacementDescription Prelude.Text
placementDescription_placementName = Lens.lens (\PlacementDescription' {placementName} -> placementName) (\s@PlacementDescription' {} a -> s {placementName = a} :: PlacementDescription)

-- | The user-defined attributes associated with the placement.
placementDescription_attributes :: Lens.Lens' PlacementDescription (Prelude.HashMap Prelude.Text Prelude.Text)
placementDescription_attributes = Lens.lens (\PlacementDescription' {attributes} -> attributes) (\s@PlacementDescription' {} a -> s {attributes = a} :: PlacementDescription) Prelude.. Lens.coerced

-- | The date when the placement was initially created, in UNIX epoch time
-- format.
placementDescription_createdDate :: Lens.Lens' PlacementDescription Prelude.UTCTime
placementDescription_createdDate = Lens.lens (\PlacementDescription' {createdDate} -> createdDate) (\s@PlacementDescription' {} a -> s {createdDate = a} :: PlacementDescription) Prelude.. Data._Time

-- | The date when the placement was last updated, in UNIX epoch time format.
-- If the placement was not updated, then @createdDate@ and @updatedDate@
-- are the same.
placementDescription_updatedDate :: Lens.Lens' PlacementDescription Prelude.UTCTime
placementDescription_updatedDate = Lens.lens (\PlacementDescription' {updatedDate} -> updatedDate) (\s@PlacementDescription' {} a -> s {updatedDate = a} :: PlacementDescription) Prelude.. Data._Time

instance Data.FromJSON PlacementDescription where
  parseJSON =
    Data.withObject
      "PlacementDescription"
      ( \x ->
          PlacementDescription'
            Prelude.<$> (x Data..: "projectName")
            Prelude.<*> (x Data..: "placementName")
            Prelude.<*> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "updatedDate")
      )

instance Prelude.Hashable PlacementDescription where
  hashWithSalt _salt PlacementDescription' {..} =
    _salt
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` placementName
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` updatedDate

instance Prelude.NFData PlacementDescription where
  rnf PlacementDescription' {..} =
    Prelude.rnf projectName `Prelude.seq`
      Prelude.rnf placementName `Prelude.seq`
        Prelude.rnf attributes `Prelude.seq`
          Prelude.rnf createdDate `Prelude.seq`
            Prelude.rnf updatedDate
