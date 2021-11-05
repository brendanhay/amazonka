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
-- Module      : Network.AWS.ElasticInference.Types.MemoryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticInference.Types.MemoryInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON MemoryInfo where
  parseJSON =
    Core.withObject
      "MemoryInfo"
      ( \x ->
          MemoryInfo' Prelude.<$> (x Core..:? "sizeInMiB")
      )

instance Prelude.Hashable MemoryInfo

instance Prelude.NFData MemoryInfo
