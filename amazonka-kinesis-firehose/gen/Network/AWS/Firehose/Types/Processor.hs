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
-- Module      : Network.AWS.Firehose.Types.Processor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Processor where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorType
import qualified Network.AWS.Lens as Lens

-- | Describes a data processor.
--
-- /See:/ 'newProcessor' smart constructor.
data Processor = Processor'
  { -- | The processor parameters.
    parameters :: Core.Maybe [ProcessorParameter],
    -- | The type of processor.
    type' :: ProcessorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Processor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'processor_parameters' - The processor parameters.
--
-- 'type'', 'processor_type' - The type of processor.
newProcessor ::
  -- | 'type''
  ProcessorType ->
  Processor
newProcessor pType_ =
  Processor'
    { parameters = Core.Nothing,
      type' = pType_
    }

-- | The processor parameters.
processor_parameters :: Lens.Lens' Processor (Core.Maybe [ProcessorParameter])
processor_parameters = Lens.lens (\Processor' {parameters} -> parameters) (\s@Processor' {} a -> s {parameters = a} :: Processor) Core.. Lens.mapping Lens._Coerce

-- | The type of processor.
processor_type :: Lens.Lens' Processor ProcessorType
processor_type = Lens.lens (\Processor' {type'} -> type') (\s@Processor' {} a -> s {type' = a} :: Processor)

instance Core.FromJSON Processor where
  parseJSON =
    Core.withObject
      "Processor"
      ( \x ->
          Processor'
            Core.<$> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable Processor

instance Core.NFData Processor

instance Core.ToJSON Processor where
  toJSON Processor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("Type" Core..= type')
          ]
      )
