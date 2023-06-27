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
-- Module      : Amazonka.ElasticInference.Types.AcceleratorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticInference.Types.AcceleratorType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticInference.Types.KeyValuePair
import Amazonka.ElasticInference.Types.MemoryInfo
import qualified Amazonka.Prelude as Prelude

-- | The details of an Elastic Inference Accelerator type.
--
-- /See:/ 'newAcceleratorType' smart constructor.
data AcceleratorType = AcceleratorType'
  { -- | The name of the Elastic Inference Accelerator type.
    acceleratorTypeName :: Prelude.Maybe Prelude.Text,
    -- | The memory information of the Elastic Inference Accelerator type.
    memoryInfo :: Prelude.Maybe MemoryInfo,
    -- | The throughput information of the Elastic Inference Accelerator type.
    throughputInfo :: Prelude.Maybe [KeyValuePair]
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
-- 'acceleratorTypeName', 'acceleratorType_acceleratorTypeName' - The name of the Elastic Inference Accelerator type.
--
-- 'memoryInfo', 'acceleratorType_memoryInfo' - The memory information of the Elastic Inference Accelerator type.
--
-- 'throughputInfo', 'acceleratorType_throughputInfo' - The throughput information of the Elastic Inference Accelerator type.
newAcceleratorType ::
  AcceleratorType
newAcceleratorType =
  AcceleratorType'
    { acceleratorTypeName =
        Prelude.Nothing,
      memoryInfo = Prelude.Nothing,
      throughputInfo = Prelude.Nothing
    }

-- | The name of the Elastic Inference Accelerator type.
acceleratorType_acceleratorTypeName :: Lens.Lens' AcceleratorType (Prelude.Maybe Prelude.Text)
acceleratorType_acceleratorTypeName = Lens.lens (\AcceleratorType' {acceleratorTypeName} -> acceleratorTypeName) (\s@AcceleratorType' {} a -> s {acceleratorTypeName = a} :: AcceleratorType)

-- | The memory information of the Elastic Inference Accelerator type.
acceleratorType_memoryInfo :: Lens.Lens' AcceleratorType (Prelude.Maybe MemoryInfo)
acceleratorType_memoryInfo = Lens.lens (\AcceleratorType' {memoryInfo} -> memoryInfo) (\s@AcceleratorType' {} a -> s {memoryInfo = a} :: AcceleratorType)

-- | The throughput information of the Elastic Inference Accelerator type.
acceleratorType_throughputInfo :: Lens.Lens' AcceleratorType (Prelude.Maybe [KeyValuePair])
acceleratorType_throughputInfo = Lens.lens (\AcceleratorType' {throughputInfo} -> throughputInfo) (\s@AcceleratorType' {} a -> s {throughputInfo = a} :: AcceleratorType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AcceleratorType where
  parseJSON =
    Data.withObject
      "AcceleratorType"
      ( \x ->
          AcceleratorType'
            Prelude.<$> (x Data..:? "acceleratorTypeName")
            Prelude.<*> (x Data..:? "memoryInfo")
            Prelude.<*> ( x
                            Data..:? "throughputInfo"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AcceleratorType where
  hashWithSalt _salt AcceleratorType' {..} =
    _salt
      `Prelude.hashWithSalt` acceleratorTypeName
      `Prelude.hashWithSalt` memoryInfo
      `Prelude.hashWithSalt` throughputInfo

instance Prelude.NFData AcceleratorType where
  rnf AcceleratorType' {..} =
    Prelude.rnf acceleratorTypeName
      `Prelude.seq` Prelude.rnf memoryInfo
      `Prelude.seq` Prelude.rnf throughputInfo
