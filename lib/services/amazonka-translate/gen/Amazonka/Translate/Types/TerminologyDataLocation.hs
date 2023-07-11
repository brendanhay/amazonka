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
-- Module      : Amazonka.Translate.Types.TerminologyDataLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TerminologyDataLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of the custom terminology data.
--
-- /See:/ 'newTerminologyDataLocation' smart constructor.
data TerminologyDataLocation = TerminologyDataLocation'
  { -- | The repository type for the custom terminology data.
    repositoryType :: Prelude.Text,
    -- | The Amazon S3 location of the most recent custom terminology input file
    -- that was successfully imported into Amazon Translate. The location is
    -- returned as a presigned URL that has a 30-minute expiration .
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
-- Create a value of 'TerminologyDataLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryType', 'terminologyDataLocation_repositoryType' - The repository type for the custom terminology data.
--
-- 'location', 'terminologyDataLocation_location' - The Amazon S3 location of the most recent custom terminology input file
-- that was successfully imported into Amazon Translate. The location is
-- returned as a presigned URL that has a 30-minute expiration .
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
newTerminologyDataLocation ::
  -- | 'repositoryType'
  Prelude.Text ->
  -- | 'location'
  Prelude.Text ->
  TerminologyDataLocation
newTerminologyDataLocation
  pRepositoryType_
  pLocation_ =
    TerminologyDataLocation'
      { repositoryType =
          pRepositoryType_,
        location = pLocation_
      }

-- | The repository type for the custom terminology data.
terminologyDataLocation_repositoryType :: Lens.Lens' TerminologyDataLocation Prelude.Text
terminologyDataLocation_repositoryType = Lens.lens (\TerminologyDataLocation' {repositoryType} -> repositoryType) (\s@TerminologyDataLocation' {} a -> s {repositoryType = a} :: TerminologyDataLocation)

-- | The Amazon S3 location of the most recent custom terminology input file
-- that was successfully imported into Amazon Translate. The location is
-- returned as a presigned URL that has a 30-minute expiration .
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
terminologyDataLocation_location :: Lens.Lens' TerminologyDataLocation Prelude.Text
terminologyDataLocation_location = Lens.lens (\TerminologyDataLocation' {location} -> location) (\s@TerminologyDataLocation' {} a -> s {location = a} :: TerminologyDataLocation)

instance Data.FromJSON TerminologyDataLocation where
  parseJSON =
    Data.withObject
      "TerminologyDataLocation"
      ( \x ->
          TerminologyDataLocation'
            Prelude.<$> (x Data..: "RepositoryType")
            Prelude.<*> (x Data..: "Location")
      )

instance Prelude.Hashable TerminologyDataLocation where
  hashWithSalt _salt TerminologyDataLocation' {..} =
    _salt
      `Prelude.hashWithSalt` repositoryType
      `Prelude.hashWithSalt` location

instance Prelude.NFData TerminologyDataLocation where
  rnf TerminologyDataLocation' {..} =
    Prelude.rnf repositoryType
      `Prelude.seq` Prelude.rnf location
