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
-- Module      : Network.AWS.SageMaker.Types.CacheHitResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CacheHitResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on the cache hit of a pipeline execution step.
--
-- /See:/ 'newCacheHitResult' smart constructor.
data CacheHitResult = CacheHitResult'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    sourcePipelineExecutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CacheHitResult where
  parseJSON =
    Prelude.withObject
      "CacheHitResult"
      ( \x ->
          CacheHitResult'
            Prelude.<$> (x Prelude..:? "SourcePipelineExecutionArn")
      )

instance Prelude.Hashable CacheHitResult

instance Prelude.NFData CacheHitResult
