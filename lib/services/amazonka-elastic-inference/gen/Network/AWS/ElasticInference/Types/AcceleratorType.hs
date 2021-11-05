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
-- Module      : Network.AWS.ElasticInference.Types.AcceleratorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Types.AcceleratorType where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticInference.Types.KeyValuePair
import Network.AWS.ElasticInference.Types.MemoryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of an Elastic Inference Accelerator type.
--
-- /See:/ 'newAcceleratorType' smart constructor.
data AcceleratorType = AcceleratorType'
  { -- | The throughput information of the Elastic Inference Accelerator type.
    throughputInfo :: Prelude.Maybe [KeyValuePair],
    -- | The memory information of the Elastic Inference Accelerator type.
    memoryInfo :: Prelude.Maybe MemoryInfo,
    -- | The name of the Elastic Inference Accelerator type.
    acceleratorTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throughputInfo', 'acceleratorType_throughputInfo' - The throughput information of the Elastic Inference Accelerator type.
--
-- 'memoryInfo', 'acceleratorType_memoryInfo' - The memory information of the Elastic Inference Accelerator type.
--
-- 'acceleratorTypeName', 'acceleratorType_acceleratorTypeName' - The name of the Elastic Inference Accelerator type.
newAcceleratorType ::
  AcceleratorType
newAcceleratorType =
  AcceleratorType'
    { throughputInfo = Prelude.Nothing,
      memoryInfo = Prelude.Nothing,
      acceleratorTypeName = Prelude.Nothing
    }

-- | The throughput information of the Elastic Inference Accelerator type.
acceleratorType_throughputInfo :: Lens.Lens' AcceleratorType (Prelude.Maybe [KeyValuePair])
acceleratorType_throughputInfo = Lens.lens (\AcceleratorType' {throughputInfo} -> throughputInfo) (\s@AcceleratorType' {} a -> s {throughputInfo = a} :: AcceleratorType) Prelude.. Lens.mapping Lens.coerced

-- | The memory information of the Elastic Inference Accelerator type.
acceleratorType_memoryInfo :: Lens.Lens' AcceleratorType (Prelude.Maybe MemoryInfo)
acceleratorType_memoryInfo = Lens.lens (\AcceleratorType' {memoryInfo} -> memoryInfo) (\s@AcceleratorType' {} a -> s {memoryInfo = a} :: AcceleratorType)

-- | The name of the Elastic Inference Accelerator type.
acceleratorType_acceleratorTypeName :: Lens.Lens' AcceleratorType (Prelude.Maybe Prelude.Text)
acceleratorType_acceleratorTypeName = Lens.lens (\AcceleratorType' {acceleratorTypeName} -> acceleratorTypeName) (\s@AcceleratorType' {} a -> s {acceleratorTypeName = a} :: AcceleratorType)

instance Core.FromJSON AcceleratorType where
  parseJSON =
    Core.withObject
      "AcceleratorType"
      ( \x ->
          AcceleratorType'
            Prelude.<$> (x Core..:? "throughputInfo" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "memoryInfo")
            Prelude.<*> (x Core..:? "acceleratorTypeName")
      )

instance Prelude.Hashable AcceleratorType

instance Prelude.NFData AcceleratorType
