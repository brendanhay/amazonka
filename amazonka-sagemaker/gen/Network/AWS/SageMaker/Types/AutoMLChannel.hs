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
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLChannel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.CompressionType

-- | Similar to Channel. A channel is a named input source that training
-- algorithms can consume. Refer to Channel for detailed descriptions.
--
-- /See:/ 'newAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | You can use Gzip or None. The default value is None.
    compressionType :: Core.Maybe CompressionType,
    -- | The data source.
    dataSource :: AutoMLDataSource,
    -- | The name of the target variable in supervised learning, a.k.a. \'y\'.
    targetAttributeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressionType', 'autoMLChannel_compressionType' - You can use Gzip or None. The default value is None.
--
-- 'dataSource', 'autoMLChannel_dataSource' - The data source.
--
-- 'targetAttributeName', 'autoMLChannel_targetAttributeName' - The name of the target variable in supervised learning, a.k.a. \'y\'.
newAutoMLChannel ::
  -- | 'dataSource'
  AutoMLDataSource ->
  -- | 'targetAttributeName'
  Core.Text ->
  AutoMLChannel
newAutoMLChannel pDataSource_ pTargetAttributeName_ =
  AutoMLChannel'
    { compressionType = Core.Nothing,
      dataSource = pDataSource_,
      targetAttributeName = pTargetAttributeName_
    }

-- | You can use Gzip or None. The default value is None.
autoMLChannel_compressionType :: Lens.Lens' AutoMLChannel (Core.Maybe CompressionType)
autoMLChannel_compressionType = Lens.lens (\AutoMLChannel' {compressionType} -> compressionType) (\s@AutoMLChannel' {} a -> s {compressionType = a} :: AutoMLChannel)

-- | The data source.
autoMLChannel_dataSource :: Lens.Lens' AutoMLChannel AutoMLDataSource
autoMLChannel_dataSource = Lens.lens (\AutoMLChannel' {dataSource} -> dataSource) (\s@AutoMLChannel' {} a -> s {dataSource = a} :: AutoMLChannel)

-- | The name of the target variable in supervised learning, a.k.a. \'y\'.
autoMLChannel_targetAttributeName :: Lens.Lens' AutoMLChannel Core.Text
autoMLChannel_targetAttributeName = Lens.lens (\AutoMLChannel' {targetAttributeName} -> targetAttributeName) (\s@AutoMLChannel' {} a -> s {targetAttributeName = a} :: AutoMLChannel)

instance Core.FromJSON AutoMLChannel where
  parseJSON =
    Core.withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            Core.<$> (x Core..:? "CompressionType")
            Core.<*> (x Core..: "DataSource")
            Core.<*> (x Core..: "TargetAttributeName")
      )

instance Core.Hashable AutoMLChannel

instance Core.NFData AutoMLChannel

instance Core.ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CompressionType" Core..=)
              Core.<$> compressionType,
            Core.Just ("DataSource" Core..= dataSource),
            Core.Just
              ("TargetAttributeName" Core..= targetAttributeName)
          ]
      )
