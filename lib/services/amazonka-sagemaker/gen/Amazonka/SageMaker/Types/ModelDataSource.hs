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
-- Module      : Amazonka.SageMaker.Types.ModelDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.S3ModelDataSource

-- | Specifies the location of ML model data to deploy. If specified, you
-- must specify one and only one of the available data sources.
--
-- /See:/ 'newModelDataSource' smart constructor.
data ModelDataSource = ModelDataSource'
  { -- | Specifies the S3 location of ML model data to deploy.
    s3DataSource :: S3ModelDataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'modelDataSource_s3DataSource' - Specifies the S3 location of ML model data to deploy.
newModelDataSource ::
  -- | 's3DataSource'
  S3ModelDataSource ->
  ModelDataSource
newModelDataSource pS3DataSource_ =
  ModelDataSource' {s3DataSource = pS3DataSource_}

-- | Specifies the S3 location of ML model data to deploy.
modelDataSource_s3DataSource :: Lens.Lens' ModelDataSource S3ModelDataSource
modelDataSource_s3DataSource = Lens.lens (\ModelDataSource' {s3DataSource} -> s3DataSource) (\s@ModelDataSource' {} a -> s {s3DataSource = a} :: ModelDataSource)

instance Data.FromJSON ModelDataSource where
  parseJSON =
    Data.withObject
      "ModelDataSource"
      ( \x ->
          ModelDataSource'
            Prelude.<$> (x Data..: "S3DataSource")
      )

instance Prelude.Hashable ModelDataSource where
  hashWithSalt _salt ModelDataSource' {..} =
    _salt `Prelude.hashWithSalt` s3DataSource

instance Prelude.NFData ModelDataSource where
  rnf ModelDataSource' {..} = Prelude.rnf s3DataSource

instance Data.ToJSON ModelDataSource where
  toJSON ModelDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3DataSource" Data..= s3DataSource)]
      )
