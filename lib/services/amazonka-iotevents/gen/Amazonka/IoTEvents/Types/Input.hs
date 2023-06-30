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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.InputConfiguration
import Amazonka.IoTEvents.Types.InputDefinition
import qualified Amazonka.Prelude as Prelude

-- | Information about the input.
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Information about the configuration of an input.
    inputConfiguration :: Prelude.Maybe InputConfiguration,
    -- | The definition of the input.
    inputDefinition :: Prelude.Maybe InputDefinition
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
-- 'inputConfiguration', 'input_inputConfiguration' - Information about the configuration of an input.
--
-- 'inputDefinition', 'input_inputDefinition' - The definition of the input.
newInput ::
  Input
newInput =
  Input'
    { inputConfiguration = Prelude.Nothing,
      inputDefinition = Prelude.Nothing
    }

-- | Information about the configuration of an input.
input_inputConfiguration :: Lens.Lens' Input (Prelude.Maybe InputConfiguration)
input_inputConfiguration = Lens.lens (\Input' {inputConfiguration} -> inputConfiguration) (\s@Input' {} a -> s {inputConfiguration = a} :: Input)

-- | The definition of the input.
input_inputDefinition :: Lens.Lens' Input (Prelude.Maybe InputDefinition)
input_inputDefinition = Lens.lens (\Input' {inputDefinition} -> inputDefinition) (\s@Input' {} a -> s {inputDefinition = a} :: Input)

instance Data.FromJSON Input where
  parseJSON =
    Data.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Data..:? "inputConfiguration")
            Prelude.<*> (x Data..:? "inputDefinition")
      )

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt
      `Prelude.hashWithSalt` inputConfiguration
      `Prelude.hashWithSalt` inputDefinition

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf inputConfiguration
      `Prelude.seq` Prelude.rnf inputDefinition
