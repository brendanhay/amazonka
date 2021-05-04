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
-- Module      : Network.AWS.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataDataLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate.
--
-- /See:/ 'newParallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { -- | Describes the repository that contains the parallel data input file.
    repositoryType :: Prelude.Text,
    -- | The Amazon S3 location of the parallel data input file. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'location'
  Prelude.Text ->
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
parallelDataDataLocation_repositoryType :: Lens.Lens' ParallelDataDataLocation Prelude.Text
parallelDataDataLocation_repositoryType = Lens.lens (\ParallelDataDataLocation' {repositoryType} -> repositoryType) (\s@ParallelDataDataLocation' {} a -> s {repositoryType = a} :: ParallelDataDataLocation)

-- | The Amazon S3 location of the parallel data input file. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
parallelDataDataLocation_location :: Lens.Lens' ParallelDataDataLocation Prelude.Text
parallelDataDataLocation_location = Lens.lens (\ParallelDataDataLocation' {location} -> location) (\s@ParallelDataDataLocation' {} a -> s {location = a} :: ParallelDataDataLocation)

instance Prelude.FromJSON ParallelDataDataLocation where
  parseJSON =
    Prelude.withObject
      "ParallelDataDataLocation"
      ( \x ->
          ParallelDataDataLocation'
            Prelude.<$> (x Prelude..: "RepositoryType")
            Prelude.<*> (x Prelude..: "Location")
      )

instance Prelude.Hashable ParallelDataDataLocation

instance Prelude.NFData ParallelDataDataLocation
