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
-- Module      : Amazonka.Forecast.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.S3Config
import qualified Amazonka.Prelude as Prelude

-- | The source of your data, an AWS Identity and Access Management (IAM)
-- role that allows Amazon Forecast to access the data and, optionally, an
-- AWS Key Management Service (KMS) key.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The path to the data stored in an Amazon Simple Storage Service (Amazon
    -- S3) bucket along with the credentials to access the data.
    s3Config :: S3Config
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
-- 's3Config', 'dataSource_s3Config' - The path to the data stored in an Amazon Simple Storage Service (Amazon
-- S3) bucket along with the credentials to access the data.
newDataSource ::
  -- | 's3Config'
  S3Config ->
  DataSource
newDataSource pS3Config_ =
  DataSource' {s3Config = pS3Config_}

-- | The path to the data stored in an Amazon Simple Storage Service (Amazon
-- S3) bucket along with the credentials to access the data.
dataSource_s3Config :: Lens.Lens' DataSource S3Config
dataSource_s3Config = Lens.lens (\DataSource' {s3Config} -> s3Config) (\s@DataSource' {} a -> s {s3Config = a} :: DataSource)

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject
      "DataSource"
      ( \x ->
          DataSource' Prelude.<$> (x Core..: "S3Config")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` s3Config

instance Prelude.NFData DataSource where
  rnf DataSource' {..} = Prelude.rnf s3Config

instance Core.ToJSON DataSource where
  toJSON DataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Config" Core..= s3Config)]
      )
