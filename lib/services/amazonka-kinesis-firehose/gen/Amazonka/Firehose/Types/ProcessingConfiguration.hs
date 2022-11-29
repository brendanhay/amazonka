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
-- Module      : Amazonka.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ProcessingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.Processor
import qualified Amazonka.Prelude as Prelude

-- | Describes a data processing configuration.
--
-- /See:/ 'newProcessingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { -- | The data processors.
    processors :: Prelude.Maybe [Processor],
    -- | Enables or disables data processing.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processors', 'processingConfiguration_processors' - The data processors.
--
-- 'enabled', 'processingConfiguration_enabled' - Enables or disables data processing.
newProcessingConfiguration ::
  ProcessingConfiguration
newProcessingConfiguration =
  ProcessingConfiguration'
    { processors =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The data processors.
processingConfiguration_processors :: Lens.Lens' ProcessingConfiguration (Prelude.Maybe [Processor])
processingConfiguration_processors = Lens.lens (\ProcessingConfiguration' {processors} -> processors) (\s@ProcessingConfiguration' {} a -> s {processors = a} :: ProcessingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Enables or disables data processing.
processingConfiguration_enabled :: Lens.Lens' ProcessingConfiguration (Prelude.Maybe Prelude.Bool)
processingConfiguration_enabled = Lens.lens (\ProcessingConfiguration' {enabled} -> enabled) (\s@ProcessingConfiguration' {} a -> s {enabled = a} :: ProcessingConfiguration)

instance Core.FromJSON ProcessingConfiguration where
  parseJSON =
    Core.withObject
      "ProcessingConfiguration"
      ( \x ->
          ProcessingConfiguration'
            Prelude.<$> (x Core..:? "Processors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Enabled")
      )

instance Prelude.Hashable ProcessingConfiguration where
  hashWithSalt _salt ProcessingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` processors
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ProcessingConfiguration where
  rnf ProcessingConfiguration' {..} =
    Prelude.rnf processors
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Processors" Core..=) Prelude.<$> processors,
            ("Enabled" Core..=) Prelude.<$> enabled
          ]
      )
