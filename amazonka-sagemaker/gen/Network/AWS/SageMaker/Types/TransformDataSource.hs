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
-- Module      : Network.AWS.SageMaker.Types.TransformDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformDataSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TransformS3DataSource

-- | Describes the location of the channel data.
--
-- /See:/ 'newTransformDataSource' smart constructor.
data TransformDataSource = TransformDataSource'
  { -- | The S3 location of the data source that is associated with a channel.
    s3DataSource :: TransformS3DataSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransformDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataSource', 'transformDataSource_s3DataSource' - The S3 location of the data source that is associated with a channel.
newTransformDataSource ::
  -- | 's3DataSource'
  TransformS3DataSource ->
  TransformDataSource
newTransformDataSource pS3DataSource_ =
  TransformDataSource' {s3DataSource = pS3DataSource_}

-- | The S3 location of the data source that is associated with a channel.
transformDataSource_s3DataSource :: Lens.Lens' TransformDataSource TransformS3DataSource
transformDataSource_s3DataSource = Lens.lens (\TransformDataSource' {s3DataSource} -> s3DataSource) (\s@TransformDataSource' {} a -> s {s3DataSource = a} :: TransformDataSource)

instance Core.FromJSON TransformDataSource where
  parseJSON =
    Core.withObject
      "TransformDataSource"
      ( \x ->
          TransformDataSource'
            Core.<$> (x Core..: "S3DataSource")
      )

instance Core.Hashable TransformDataSource

instance Core.NFData TransformDataSource

instance Core.ToJSON TransformDataSource where
  toJSON TransformDataSource' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("S3DataSource" Core..= s3DataSource)]
      )
