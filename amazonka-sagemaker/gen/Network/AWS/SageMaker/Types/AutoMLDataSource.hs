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
-- Module      : Network.AWS.SageMaker.Types.AutoMLDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLDataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLS3DataSource

-- | The data source for the Autopilot job.
--
-- /See:/ 'newAutoMLDataSource' smart constructor.
data AutoMLDataSource = AutoMLDataSource'
  { -- | The Amazon S3 location of the input data.
    --
    -- The input data must be in CSV format and contain at least 500 rows.
    s3DataSource :: AutoMLS3DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoMLDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'autoMLDataSource_s3DataSource' - The Amazon S3 location of the input data.
--
-- The input data must be in CSV format and contain at least 500 rows.
newAutoMLDataSource ::
  -- | 's3DataSource'
  AutoMLS3DataSource ->
  AutoMLDataSource
newAutoMLDataSource pS3DataSource_ =
  AutoMLDataSource' {s3DataSource = pS3DataSource_}

-- | The Amazon S3 location of the input data.
--
-- The input data must be in CSV format and contain at least 500 rows.
autoMLDataSource_s3DataSource :: Lens.Lens' AutoMLDataSource AutoMLS3DataSource
autoMLDataSource_s3DataSource = Lens.lens (\AutoMLDataSource' {s3DataSource} -> s3DataSource) (\s@AutoMLDataSource' {} a -> s {s3DataSource = a} :: AutoMLDataSource)

instance Prelude.FromJSON AutoMLDataSource where
  parseJSON =
    Prelude.withObject
      "AutoMLDataSource"
      ( \x ->
          AutoMLDataSource'
            Prelude.<$> (x Prelude..: "S3DataSource")
      )

instance Prelude.Hashable AutoMLDataSource

instance Prelude.NFData AutoMLDataSource

instance Prelude.ToJSON AutoMLDataSource where
  toJSON AutoMLDataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("S3DataSource" Prelude..= s3DataSource)
          ]
      )
