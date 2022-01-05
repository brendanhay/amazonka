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
-- Module      : Amazonka.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.CompressionType

-- | A channel is a named input source that training algorithms can consume.
-- For more information, see .
--
-- /See:/ 'newAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | You can use @Gzip@ or @None@. The default value is @None@.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The data source for an AutoML channel.
    dataSource :: AutoMLDataSource,
    -- | The name of the target variable in supervised learning, usually
    -- represented by \'y\'.
    targetAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressionType', 'autoMLChannel_compressionType' - You can use @Gzip@ or @None@. The default value is @None@.
--
-- 'dataSource', 'autoMLChannel_dataSource' - The data source for an AutoML channel.
--
-- 'targetAttributeName', 'autoMLChannel_targetAttributeName' - The name of the target variable in supervised learning, usually
-- represented by \'y\'.
newAutoMLChannel ::
  -- | 'dataSource'
  AutoMLDataSource ->
  -- | 'targetAttributeName'
  Prelude.Text ->
  AutoMLChannel
newAutoMLChannel pDataSource_ pTargetAttributeName_ =
  AutoMLChannel'
    { compressionType = Prelude.Nothing,
      dataSource = pDataSource_,
      targetAttributeName = pTargetAttributeName_
    }

-- | You can use @Gzip@ or @None@. The default value is @None@.
autoMLChannel_compressionType :: Lens.Lens' AutoMLChannel (Prelude.Maybe CompressionType)
autoMLChannel_compressionType = Lens.lens (\AutoMLChannel' {compressionType} -> compressionType) (\s@AutoMLChannel' {} a -> s {compressionType = a} :: AutoMLChannel)

-- | The data source for an AutoML channel.
autoMLChannel_dataSource :: Lens.Lens' AutoMLChannel AutoMLDataSource
autoMLChannel_dataSource = Lens.lens (\AutoMLChannel' {dataSource} -> dataSource) (\s@AutoMLChannel' {} a -> s {dataSource = a} :: AutoMLChannel)

-- | The name of the target variable in supervised learning, usually
-- represented by \'y\'.
autoMLChannel_targetAttributeName :: Lens.Lens' AutoMLChannel Prelude.Text
autoMLChannel_targetAttributeName = Lens.lens (\AutoMLChannel' {targetAttributeName} -> targetAttributeName) (\s@AutoMLChannel' {} a -> s {targetAttributeName = a} :: AutoMLChannel)

instance Core.FromJSON AutoMLChannel where
  parseJSON =
    Core.withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            Prelude.<$> (x Core..:? "CompressionType")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "TargetAttributeName")
      )

instance Prelude.Hashable AutoMLChannel where
  hashWithSalt _salt AutoMLChannel' {..} =
    _salt `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` targetAttributeName

instance Prelude.NFData AutoMLChannel where
  rnf AutoMLChannel' {..} =
    Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf targetAttributeName

instance Core.ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CompressionType" Core..=)
              Prelude.<$> compressionType,
            Prelude.Just ("DataSource" Core..= dataSource),
            Prelude.Just
              ("TargetAttributeName" Core..= targetAttributeName)
          ]
      )
