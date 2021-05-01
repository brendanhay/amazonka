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
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLChannel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.CompressionType

-- | Similar to Channel. A channel is a named input source that training
-- algorithms can consume. Refer to Channel for detailed descriptions.
--
-- /See:/ 'newAutoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { -- | You can use Gzip or None. The default value is None.
    compressionType :: Prelude.Maybe CompressionType,
    -- | The data source.
    dataSource :: AutoMLDataSource,
    -- | The name of the target variable in supervised learning, a.k.a. \'y\'.
    targetAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AutoMLChannel
newAutoMLChannel pDataSource_ pTargetAttributeName_ =
  AutoMLChannel'
    { compressionType = Prelude.Nothing,
      dataSource = pDataSource_,
      targetAttributeName = pTargetAttributeName_
    }

-- | You can use Gzip or None. The default value is None.
autoMLChannel_compressionType :: Lens.Lens' AutoMLChannel (Prelude.Maybe CompressionType)
autoMLChannel_compressionType = Lens.lens (\AutoMLChannel' {compressionType} -> compressionType) (\s@AutoMLChannel' {} a -> s {compressionType = a} :: AutoMLChannel)

-- | The data source.
autoMLChannel_dataSource :: Lens.Lens' AutoMLChannel AutoMLDataSource
autoMLChannel_dataSource = Lens.lens (\AutoMLChannel' {dataSource} -> dataSource) (\s@AutoMLChannel' {} a -> s {dataSource = a} :: AutoMLChannel)

-- | The name of the target variable in supervised learning, a.k.a. \'y\'.
autoMLChannel_targetAttributeName :: Lens.Lens' AutoMLChannel Prelude.Text
autoMLChannel_targetAttributeName = Lens.lens (\AutoMLChannel' {targetAttributeName} -> targetAttributeName) (\s@AutoMLChannel' {} a -> s {targetAttributeName = a} :: AutoMLChannel)

instance Prelude.FromJSON AutoMLChannel where
  parseJSON =
    Prelude.withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            Prelude.<$> (x Prelude..:? "CompressionType")
            Prelude.<*> (x Prelude..: "DataSource")
            Prelude.<*> (x Prelude..: "TargetAttributeName")
      )

instance Prelude.Hashable AutoMLChannel

instance Prelude.NFData AutoMLChannel

instance Prelude.ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CompressionType" Prelude..=)
              Prelude.<$> compressionType,
            Prelude.Just ("DataSource" Prelude..= dataSource),
            Prelude.Just
              ( "TargetAttributeName"
                  Prelude..= targetAttributeName
              )
          ]
      )
