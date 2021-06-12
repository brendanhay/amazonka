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
-- Module      : Network.AWS.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessingConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.Processor
import qualified Network.AWS.Lens as Lens

-- | Describes a data processing configuration.
--
-- /See:/ 'newProcessingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { -- | Enables or disables data processing.
    enabled :: Core.Maybe Core.Bool,
    -- | The data processors.
    processors :: Core.Maybe [Processor]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProcessingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'processingConfiguration_enabled' - Enables or disables data processing.
--
-- 'processors', 'processingConfiguration_processors' - The data processors.
newProcessingConfiguration ::
  ProcessingConfiguration
newProcessingConfiguration =
  ProcessingConfiguration'
    { enabled = Core.Nothing,
      processors = Core.Nothing
    }

-- | Enables or disables data processing.
processingConfiguration_enabled :: Lens.Lens' ProcessingConfiguration (Core.Maybe Core.Bool)
processingConfiguration_enabled = Lens.lens (\ProcessingConfiguration' {enabled} -> enabled) (\s@ProcessingConfiguration' {} a -> s {enabled = a} :: ProcessingConfiguration)

-- | The data processors.
processingConfiguration_processors :: Lens.Lens' ProcessingConfiguration (Core.Maybe [Processor])
processingConfiguration_processors = Lens.lens (\ProcessingConfiguration' {processors} -> processors) (\s@ProcessingConfiguration' {} a -> s {processors = a} :: ProcessingConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ProcessingConfiguration where
  parseJSON =
    Core.withObject
      "ProcessingConfiguration"
      ( \x ->
          ProcessingConfiguration'
            Core.<$> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Processors" Core..!= Core.mempty)
      )

instance Core.Hashable ProcessingConfiguration

instance Core.NFData ProcessingConfiguration

instance Core.ToJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("Processors" Core..=) Core.<$> processors
          ]
      )
