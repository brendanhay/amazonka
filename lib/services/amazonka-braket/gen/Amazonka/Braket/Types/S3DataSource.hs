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
-- Module      : Amazonka.Braket.Types.S3DataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.S3DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the data stored in Amazon S3 used by the Amazon Braket
-- job.
--
-- /See:/ 'newS3DataSource' smart constructor.
data S3DataSource = S3DataSource'
  { -- | Depending on the value specified for the @S3DataType@, identifies either
    -- a key name prefix or a manifest that locates the S3 data source.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 's3DataSource_s3Uri' - Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest that locates the S3 data source.
newS3DataSource ::
  -- | 's3Uri'
  Prelude.Text ->
  S3DataSource
newS3DataSource pS3Uri_ =
  S3DataSource' {s3Uri = pS3Uri_}

-- | Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest that locates the S3 data source.
s3DataSource_s3Uri :: Lens.Lens' S3DataSource Prelude.Text
s3DataSource_s3Uri = Lens.lens (\S3DataSource' {s3Uri} -> s3Uri) (\s@S3DataSource' {} a -> s {s3Uri = a} :: S3DataSource)

instance Data.FromJSON S3DataSource where
  parseJSON =
    Data.withObject
      "S3DataSource"
      ( \x ->
          S3DataSource' Prelude.<$> (x Data..: "s3Uri")
      )

instance Prelude.Hashable S3DataSource where
  hashWithSalt _salt S3DataSource' {..} =
    _salt `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData S3DataSource where
  rnf S3DataSource' {..} = Prelude.rnf s3Uri

instance Data.ToJSON S3DataSource where
  toJSON S3DataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3Uri" Data..= s3Uri)]
      )
