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
-- Module      : Amazonka.IoTEvents.Types.Input
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.InputConfiguration
import Amazonka.IoTEvents.Types.InputDefinition
import qualified Amazonka.Prelude as Prelude

-- | Information about the input.
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | The definition of the input.
    inputDefinition :: Prelude.Maybe InputDefinition,
    -- | Information about the configuration of an input.
    inputConfiguration :: Prelude.Maybe InputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDefinition', 'input_inputDefinition' - The definition of the input.
--
-- 'inputConfiguration', 'input_inputConfiguration' - Information about the configuration of an input.
newInput ::
  Input
newInput =
  Input'
    { inputDefinition = Prelude.Nothing,
      inputConfiguration = Prelude.Nothing
    }

-- | The definition of the input.
input_inputDefinition :: Lens.Lens' Input (Prelude.Maybe InputDefinition)
input_inputDefinition = Lens.lens (\Input' {inputDefinition} -> inputDefinition) (\s@Input' {} a -> s {inputDefinition = a} :: Input)

-- | Information about the configuration of an input.
input_inputConfiguration :: Lens.Lens' Input (Prelude.Maybe InputConfiguration)
input_inputConfiguration = Lens.lens (\Input' {inputConfiguration} -> inputConfiguration) (\s@Input' {} a -> s {inputConfiguration = a} :: Input)

instance Core.FromJSON Input where
  parseJSON =
    Core.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Core..:? "inputDefinition")
            Prelude.<*> (x Core..:? "inputConfiguration")
      )

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt `Prelude.hashWithSalt` inputDefinition
      `Prelude.hashWithSalt` inputConfiguration

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf inputDefinition
      `Prelude.seq` Prelude.rnf inputConfiguration
