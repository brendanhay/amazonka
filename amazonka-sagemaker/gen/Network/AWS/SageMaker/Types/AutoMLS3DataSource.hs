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
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AutoMLS3DataType

-- | The Amazon S3 data source.
--
-- /See:/ 'newAutoMLS3DataSource' smart constructor.
data AutoMLS3DataSource = AutoMLS3DataSource'
  { -- | The data type.
    s3DataType :: AutoMLS3DataType,
    -- | The URL to the Amazon S3 data source.
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLS3DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataType', 'autoMLS3DataSource_s3DataType' - The data type.
--
-- 's3Uri', 'autoMLS3DataSource_s3Uri' - The URL to the Amazon S3 data source.
newAutoMLS3DataSource ::
  -- | 's3DataType'
  AutoMLS3DataType ->
  -- | 's3Uri'
  Core.Text ->
  AutoMLS3DataSource
newAutoMLS3DataSource pS3DataType_ pS3Uri_ =
  AutoMLS3DataSource'
    { s3DataType = pS3DataType_,
      s3Uri = pS3Uri_
    }

-- | The data type.
autoMLS3DataSource_s3DataType :: Lens.Lens' AutoMLS3DataSource AutoMLS3DataType
autoMLS3DataSource_s3DataType = Lens.lens (\AutoMLS3DataSource' {s3DataType} -> s3DataType) (\s@AutoMLS3DataSource' {} a -> s {s3DataType = a} :: AutoMLS3DataSource)

-- | The URL to the Amazon S3 data source.
autoMLS3DataSource_s3Uri :: Lens.Lens' AutoMLS3DataSource Core.Text
autoMLS3DataSource_s3Uri = Lens.lens (\AutoMLS3DataSource' {s3Uri} -> s3Uri) (\s@AutoMLS3DataSource' {} a -> s {s3Uri = a} :: AutoMLS3DataSource)

instance Core.FromJSON AutoMLS3DataSource where
  parseJSON =
    Core.withObject
      "AutoMLS3DataSource"
      ( \x ->
          AutoMLS3DataSource'
            Core.<$> (x Core..: "S3DataType")
            Core.<*> (x Core..: "S3Uri")
      )

instance Core.Hashable AutoMLS3DataSource

instance Core.NFData AutoMLS3DataSource

instance Core.ToJSON AutoMLS3DataSource where
  toJSON AutoMLS3DataSource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3DataType" Core..= s3DataType),
            Core.Just ("S3Uri" Core..= s3Uri)
          ]
      )
