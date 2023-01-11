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
-- Module      : Amazonka.SageMaker.Types.OutputParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OutputParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An output parameter of a pipeline step.
--
-- /See:/ 'newOutputParameter' smart constructor.
data OutputParameter = OutputParameter'
  { -- | The name of the output parameter.
    name :: Prelude.Text,
    -- | The value of the output parameter.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'outputParameter_name' - The name of the output parameter.
--
-- 'value', 'outputParameter_value' - The value of the output parameter.
newOutputParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  OutputParameter
newOutputParameter pName_ pValue_ =
  OutputParameter' {name = pName_, value = pValue_}

-- | The name of the output parameter.
outputParameter_name :: Lens.Lens' OutputParameter Prelude.Text
outputParameter_name = Lens.lens (\OutputParameter' {name} -> name) (\s@OutputParameter' {} a -> s {name = a} :: OutputParameter)

-- | The value of the output parameter.
outputParameter_value :: Lens.Lens' OutputParameter Prelude.Text
outputParameter_value = Lens.lens (\OutputParameter' {value} -> value) (\s@OutputParameter' {} a -> s {value = a} :: OutputParameter)

instance Data.FromJSON OutputParameter where
  parseJSON =
    Data.withObject
      "OutputParameter"
      ( \x ->
          OutputParameter'
            Prelude.<$> (x Data..: "Name") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable OutputParameter where
  hashWithSalt _salt OutputParameter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData OutputParameter where
  rnf OutputParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON OutputParameter where
  toJSON OutputParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
