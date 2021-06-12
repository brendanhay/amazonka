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
-- Module      : Network.AWS.SageMaker.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Parameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Assigns a value to a named Pipeline parameter.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The name of the parameter to assign a value to. This parameter name must
    -- match a named parameter in the pipeline definition.
    name :: Core.Text,
    -- | The literal value for the parameter.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parameter_name' - The name of the parameter to assign a value to. This parameter name must
-- match a named parameter in the pipeline definition.
--
-- 'value', 'parameter_value' - The literal value for the parameter.
newParameter ::
  -- | 'name'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  Parameter
newParameter pName_ pValue_ =
  Parameter' {name = pName_, value = pValue_}

-- | The name of the parameter to assign a value to. This parameter name must
-- match a named parameter in the pipeline definition.
parameter_name :: Lens.Lens' Parameter Core.Text
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | The literal value for the parameter.
parameter_value :: Lens.Lens' Parameter Core.Text
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter)

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Value")
      )

instance Core.Hashable Parameter

instance Core.NFData Parameter

instance Core.ToJSON Parameter where
  toJSON Parameter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Value" Core..= value)
          ]
      )
