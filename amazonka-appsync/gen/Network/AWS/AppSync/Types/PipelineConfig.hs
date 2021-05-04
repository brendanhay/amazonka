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
-- Module      : Network.AWS.AppSync.Types.PipelineConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.PipelineConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The pipeline configuration for a resolver of kind @PIPELINE@.
--
-- /See:/ 'newPipelineConfig' smart constructor.
data PipelineConfig = PipelineConfig'
  { -- | A list of @Function@ objects.
    functions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PipelineConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functions', 'pipelineConfig_functions' - A list of @Function@ objects.
newPipelineConfig ::
  PipelineConfig
newPipelineConfig =
  PipelineConfig' {functions = Prelude.Nothing}

-- | A list of @Function@ objects.
pipelineConfig_functions :: Lens.Lens' PipelineConfig (Prelude.Maybe [Prelude.Text])
pipelineConfig_functions = Lens.lens (\PipelineConfig' {functions} -> functions) (\s@PipelineConfig' {} a -> s {functions = a} :: PipelineConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON PipelineConfig where
  parseJSON =
    Prelude.withObject
      "PipelineConfig"
      ( \x ->
          PipelineConfig'
            Prelude.<$> ( x Prelude..:? "functions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PipelineConfig

instance Prelude.NFData PipelineConfig

instance Prelude.ToJSON PipelineConfig where
  toJSON PipelineConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("functions" Prelude..=) Prelude.<$> functions]
      )
