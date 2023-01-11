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
-- Module      : Amazonka.SageMaker.Types.CacheHitResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CacheHitResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on the cache hit of a pipeline execution step.
--
-- /See:/ 'newCacheHitResult' smart constructor.
data CacheHitResult = CacheHitResult'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    sourcePipelineExecutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheHitResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePipelineExecutionArn', 'cacheHitResult_sourcePipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newCacheHitResult ::
  CacheHitResult
newCacheHitResult =
  CacheHitResult'
    { sourcePipelineExecutionArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
cacheHitResult_sourcePipelineExecutionArn :: Lens.Lens' CacheHitResult (Prelude.Maybe Prelude.Text)
cacheHitResult_sourcePipelineExecutionArn = Lens.lens (\CacheHitResult' {sourcePipelineExecutionArn} -> sourcePipelineExecutionArn) (\s@CacheHitResult' {} a -> s {sourcePipelineExecutionArn = a} :: CacheHitResult)

instance Data.FromJSON CacheHitResult where
  parseJSON =
    Data.withObject
      "CacheHitResult"
      ( \x ->
          CacheHitResult'
            Prelude.<$> (x Data..:? "SourcePipelineExecutionArn")
      )

instance Prelude.Hashable CacheHitResult where
  hashWithSalt _salt CacheHitResult' {..} =
    _salt
      `Prelude.hashWithSalt` sourcePipelineExecutionArn

instance Prelude.NFData CacheHitResult where
  rnf CacheHitResult' {..} =
    Prelude.rnf sourcePipelineExecutionArn
