{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Location where

import Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the location of a change or comment in the
-- comparison between two commits or a pull request.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The name of the file being compared, including its extension and
    -- subdirectory, if any.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The position of a change in a compared file, in line number format.
    filePosition :: Prelude.Maybe Prelude.Integer,
    -- | In a comparison of commits or a pull request, whether the change is in
    -- the before or after of that comparison.
    relativeFileVersion :: Prelude.Maybe RelativeFileVersionEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'location_filePath' - The name of the file being compared, including its extension and
-- subdirectory, if any.
--
-- 'filePosition', 'location_filePosition' - The position of a change in a compared file, in line number format.
--
-- 'relativeFileVersion', 'location_relativeFileVersion' - In a comparison of commits or a pull request, whether the change is in
-- the before or after of that comparison.
newLocation ::
  Location
newLocation =
  Location'
    { filePath = Prelude.Nothing,
      filePosition = Prelude.Nothing,
      relativeFileVersion = Prelude.Nothing
    }

-- | The name of the file being compared, including its extension and
-- subdirectory, if any.
location_filePath :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_filePath = Lens.lens (\Location' {filePath} -> filePath) (\s@Location' {} a -> s {filePath = a} :: Location)

-- | The position of a change in a compared file, in line number format.
location_filePosition :: Lens.Lens' Location (Prelude.Maybe Prelude.Integer)
location_filePosition = Lens.lens (\Location' {filePosition} -> filePosition) (\s@Location' {} a -> s {filePosition = a} :: Location)

-- | In a comparison of commits or a pull request, whether the change is in
-- the before or after of that comparison.
location_relativeFileVersion :: Lens.Lens' Location (Prelude.Maybe RelativeFileVersionEnum)
location_relativeFileVersion = Lens.lens (\Location' {relativeFileVersion} -> relativeFileVersion) (\s@Location' {} a -> s {relativeFileVersion = a} :: Location)

instance Prelude.FromJSON Location where
  parseJSON =
    Prelude.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Prelude..:? "filePath")
            Prelude.<*> (x Prelude..:? "filePosition")
            Prelude.<*> (x Prelude..:? "relativeFileVersion")
      )

instance Prelude.Hashable Location

instance Prelude.NFData Location

instance Prelude.ToJSON Location where
  toJSON Location' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("filePath" Prelude..=) Prelude.<$> filePath,
            ("filePosition" Prelude..=) Prelude.<$> filePosition,
            ("relativeFileVersion" Prelude..=)
              Prelude.<$> relativeFileVersion
          ]
      )
