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
-- Module      : Amazonka.SageMaker.Types.AutoMLDataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLS3DataSource

-- | The data source for the Autopilot job.
--
-- /See:/ 'newAutoMLDataSource' smart constructor.
data AutoMLDataSource = AutoMLDataSource'
  { -- | The Amazon S3 location of the input data.
    s3DataSource :: AutoMLS3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'autoMLDataSource_s3DataSource' - The Amazon S3 location of the input data.
newAutoMLDataSource ::
  -- | 's3DataSource'
  AutoMLS3DataSource ->
  AutoMLDataSource
newAutoMLDataSource pS3DataSource_ =
  AutoMLDataSource' {s3DataSource = pS3DataSource_}

-- | The Amazon S3 location of the input data.
autoMLDataSource_s3DataSource :: Lens.Lens' AutoMLDataSource AutoMLS3DataSource
autoMLDataSource_s3DataSource = Lens.lens (\AutoMLDataSource' {s3DataSource} -> s3DataSource) (\s@AutoMLDataSource' {} a -> s {s3DataSource = a} :: AutoMLDataSource)

instance Data.FromJSON AutoMLDataSource where
  parseJSON =
    Data.withObject
      "AutoMLDataSource"
      ( \x ->
          AutoMLDataSource'
            Prelude.<$> (x Data..: "S3DataSource")
      )

instance Prelude.Hashable AutoMLDataSource where
  hashWithSalt _salt AutoMLDataSource' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource

instance Prelude.NFData AutoMLDataSource where
  rnf AutoMLDataSource' {..} = Prelude.rnf s3DataSource

instance Data.ToJSON AutoMLDataSource where
  toJSON AutoMLDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3DataSource" Data..= s3DataSource)]
      )
