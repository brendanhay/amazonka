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
-- Module      : Amazonka.Braket.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.DataSource where

import Amazonka.Braket.Types.S3DataSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the source of the data used by the Amazon Braket job.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | Information about the data stored in Amazon S3 used by the Amazon Braket
    -- job.
    s3DataSource :: S3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'dataSource_s3DataSource' - Information about the data stored in Amazon S3 used by the Amazon Braket
-- job.
newDataSource ::
  -- | 's3DataSource'
  S3DataSource ->
  DataSource
newDataSource pS3DataSource_ =
  DataSource' {s3DataSource = pS3DataSource_}

-- | Information about the data stored in Amazon S3 used by the Amazon Braket
-- job.
dataSource_s3DataSource :: Lens.Lens' DataSource S3DataSource
dataSource_s3DataSource = Lens.lens (\DataSource' {s3DataSource} -> s3DataSource) (\s@DataSource' {} a -> s {s3DataSource = a} :: DataSource)

instance Data.FromJSON DataSource where
  parseJSON =
    Data.withObject
      "DataSource"
      ( \x ->
          DataSource' Prelude.<$> (x Data..: "s3DataSource")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource

instance Prelude.NFData DataSource where
  rnf DataSource' {..} = Prelude.rnf s3DataSource

instance Data.ToJSON DataSource where
  toJSON DataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3DataSource" Data..= s3DataSource)]
      )
