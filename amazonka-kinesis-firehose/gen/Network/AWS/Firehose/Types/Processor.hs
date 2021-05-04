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
-- Module      : Network.AWS.Firehose.Types.Processor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Processor where

import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a data processor.
--
-- /See:/ 'newProcessor' smart constructor.
data Processor = Processor'
  { -- | The processor parameters.
    parameters :: Prelude.Maybe [ProcessorParameter],
    -- | The type of processor.
    type' :: ProcessorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { parameters = Prelude.Nothing,
      type' = pType_
    }

-- | The processor parameters.
processor_parameters :: Lens.Lens' Processor (Prelude.Maybe [ProcessorParameter])
processor_parameters = Lens.lens (\Processor' {parameters} -> parameters) (\s@Processor' {} a -> s {parameters = a} :: Processor) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of processor.
processor_type :: Lens.Lens' Processor ProcessorType
processor_type = Lens.lens (\Processor' {type'} -> type') (\s@Processor' {} a -> s {type' = a} :: Processor)

instance Prelude.FromJSON Processor where
  parseJSON =
    Prelude.withObject
      "Processor"
      ( \x ->
          Processor'
            Prelude.<$> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable Processor

instance Prelude.NFData Processor

instance Prelude.ToJSON Processor where
  toJSON Processor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Parameters" Prelude..=) Prelude.<$> parameters,
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
