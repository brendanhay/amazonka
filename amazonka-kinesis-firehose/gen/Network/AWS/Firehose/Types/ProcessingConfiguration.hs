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
-- Module      : Network.AWS.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessingConfiguration where

import Network.AWS.Firehose.Types.Processor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a data processing configuration.
--
-- /See:/ 'newProcessingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { -- | Enables or disables data processing.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The data processors.
    processors :: Prelude.Maybe [Processor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { enabled = Prelude.Nothing,
      processors = Prelude.Nothing
    }

-- | Enables or disables data processing.
processingConfiguration_enabled :: Lens.Lens' ProcessingConfiguration (Prelude.Maybe Prelude.Bool)
processingConfiguration_enabled = Lens.lens (\ProcessingConfiguration' {enabled} -> enabled) (\s@ProcessingConfiguration' {} a -> s {enabled = a} :: ProcessingConfiguration)

-- | The data processors.
processingConfiguration_processors :: Lens.Lens' ProcessingConfiguration (Prelude.Maybe [Processor])
processingConfiguration_processors = Lens.lens (\ProcessingConfiguration' {processors} -> processors) (\s@ProcessingConfiguration' {} a -> s {processors = a} :: ProcessingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ProcessingConfiguration where
  parseJSON =
    Prelude.withObject
      "ProcessingConfiguration"
      ( \x ->
          ProcessingConfiguration'
            Prelude.<$> (x Prelude..:? "Enabled")
            Prelude.<*> ( x Prelude..:? "Processors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProcessingConfiguration

instance Prelude.NFData ProcessingConfiguration

instance Prelude.ToJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("Processors" Prelude..=) Prelude.<$> processors
          ]
      )
