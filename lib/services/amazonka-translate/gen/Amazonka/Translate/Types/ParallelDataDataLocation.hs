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
-- Module      : Amazonka.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.ParallelDataDataLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate.
--
-- /See:/ 'newParallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { -- | Describes the repository that contains the parallel data input file.
    repositoryType :: Prelude.Text,
    -- | The Amazon S3 location of the parallel data input file. The location is
    -- returned as a presigned URL to that has a 30-minute expiration.
    --
    -- Amazon Translate doesn\'t scan all input files for the risk of CSV
    -- injection attacks.
    --
    -- CSV injection occurs when a .csv or .tsv file is altered so that a
    -- record contains malicious code. The record begins with a special
    -- character, such as =, +, -, or \@. When the file is opened in a
    -- spreadsheet program, the program might interpret the record as a formula
    -- and run the code within it.
    --
    -- Before you download an input file from Amazon S3, ensure that you
    -- recognize the file and trust its creator.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- returned as a presigned URL to that has a 30-minute expiration.
--
-- Amazon Translate doesn\'t scan all input files for the risk of CSV
-- injection attacks.
--
-- CSV injection occurs when a .csv or .tsv file is altered so that a
-- record contains malicious code. The record begins with a special
-- character, such as =, +, -, or \@. When the file is opened in a
-- spreadsheet program, the program might interpret the record as a formula
-- and run the code within it.
--
-- Before you download an input file from Amazon S3, ensure that you
-- recognize the file and trust its creator.
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
-- returned as a presigned URL to that has a 30-minute expiration.
--
-- Amazon Translate doesn\'t scan all input files for the risk of CSV
-- injection attacks.
--
-- CSV injection occurs when a .csv or .tsv file is altered so that a
-- record contains malicious code. The record begins with a special
-- character, such as =, +, -, or \@. When the file is opened in a
-- spreadsheet program, the program might interpret the record as a formula
-- and run the code within it.
--
-- Before you download an input file from Amazon S3, ensure that you
-- recognize the file and trust its creator.
parallelDataDataLocation_location :: Lens.Lens' ParallelDataDataLocation Prelude.Text
parallelDataDataLocation_location = Lens.lens (\ParallelDataDataLocation' {location} -> location) (\s@ParallelDataDataLocation' {} a -> s {location = a} :: ParallelDataDataLocation)

instance Data.FromJSON ParallelDataDataLocation where
  parseJSON =
    Data.withObject
      "ParallelDataDataLocation"
      ( \x ->
          ParallelDataDataLocation'
            Prelude.<$> (x Data..: "RepositoryType")
            Prelude.<*> (x Data..: "Location")
      )

instance Prelude.Hashable ParallelDataDataLocation where
  hashWithSalt _salt ParallelDataDataLocation' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryType
      `Prelude.hashWithSalt` location

instance Prelude.NFData ParallelDataDataLocation where
  rnf ParallelDataDataLocation' {..} =
    Prelude.rnf repositoryType `Prelude.seq`
      Prelude.rnf location
