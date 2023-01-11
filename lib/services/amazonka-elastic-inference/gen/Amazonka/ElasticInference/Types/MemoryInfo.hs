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
-- Module      : Amazonka.ElasticInference.Types.MemoryInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticInference.Types.MemoryInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The memory information of an Elastic Inference Accelerator type.
--
-- /See:/ 'newMemoryInfo' smart constructor.
data MemoryInfo = MemoryInfo'
  { -- | The size in mebibytes of the Elastic Inference Accelerator type.
    sizeInMiB :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemoryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMiB', 'memoryInfo_sizeInMiB' - The size in mebibytes of the Elastic Inference Accelerator type.
newMemoryInfo ::
  MemoryInfo
newMemoryInfo =
  MemoryInfo' {sizeInMiB = Prelude.Nothing}

-- | The size in mebibytes of the Elastic Inference Accelerator type.
memoryInfo_sizeInMiB :: Lens.Lens' MemoryInfo (Prelude.Maybe Prelude.Int)
memoryInfo_sizeInMiB = Lens.lens (\MemoryInfo' {sizeInMiB} -> sizeInMiB) (\s@MemoryInfo' {} a -> s {sizeInMiB = a} :: MemoryInfo)

instance Data.FromJSON MemoryInfo where
  parseJSON =
    Data.withObject
      "MemoryInfo"
      ( \x ->
          MemoryInfo' Prelude.<$> (x Data..:? "sizeInMiB")
      )

instance Prelude.Hashable MemoryInfo where
  hashWithSalt _salt MemoryInfo' {..} =
    _salt `Prelude.hashWithSalt` sizeInMiB

instance Prelude.NFData MemoryInfo where
  rnf MemoryInfo' {..} = Prelude.rnf sizeInMiB
