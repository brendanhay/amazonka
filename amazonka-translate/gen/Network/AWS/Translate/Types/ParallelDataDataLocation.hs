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
-- Module      : Network.AWS.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataDataLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate.
--
-- /See:/ 'newParallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { -- | Describes the repository that contains the parallel data input file.
    repositoryType :: Core.Text,
    -- | The Amazon S3 location of the parallel data input file. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParallelDataDataLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryType', 'parallelDataDataLocation_repositoryType' - Describes the repository that contains the parallel data input file.
--
-- 'location', 'parallelDataDataLocation_location' - The Amazon S3 location of the parallel data input file. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
newParallelDataDataLocation ::
  -- | 'repositoryType'
  Core.Text ->
  -- | 'location'
  Core.Text ->
  ParallelDataDataLocation
newParallelDataDataLocation
  pRepositoryType_
  pLocation_ =
    ParallelDataDataLocation'
      { repositoryType =
          pRepositoryType_,
        location = pLocation_
      }

-- | Describes the repository that contains the parallel data input file.
parallelDataDataLocation_repositoryType :: Lens.Lens' ParallelDataDataLocation Core.Text
parallelDataDataLocation_repositoryType = Lens.lens (\ParallelDataDataLocation' {repositoryType} -> repositoryType) (\s@ParallelDataDataLocation' {} a -> s {repositoryType = a} :: ParallelDataDataLocation)

-- | The Amazon S3 location of the parallel data input file. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
parallelDataDataLocation_location :: Lens.Lens' ParallelDataDataLocation Core.Text
parallelDataDataLocation_location = Lens.lens (\ParallelDataDataLocation' {location} -> location) (\s@ParallelDataDataLocation' {} a -> s {location = a} :: ParallelDataDataLocation)

instance Core.FromJSON ParallelDataDataLocation where
  parseJSON =
    Core.withObject
      "ParallelDataDataLocation"
      ( \x ->
          ParallelDataDataLocation'
            Core.<$> (x Core..: "RepositoryType")
            Core.<*> (x Core..: "Location")
      )

instance Core.Hashable ParallelDataDataLocation

instance Core.NFData ParallelDataDataLocation
