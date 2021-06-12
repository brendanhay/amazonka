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
-- Module      : Network.AWS.ServiceCatalog.Types.ExecutionParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ExecutionParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of an execution parameter value that is passed to a self-service
-- action when executed on a provisioned product.
--
-- /See:/ 'newExecutionParameter' smart constructor.
data ExecutionParameter = ExecutionParameter'
  { -- | The name of the execution parameter.
    name :: Core.Maybe Core.Text,
    -- | The default values for the execution parameter.
    defaultValues :: Core.Maybe [Core.Text],
    -- | The execution parameter type.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecutionParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'executionParameter_name' - The name of the execution parameter.
--
-- 'defaultValues', 'executionParameter_defaultValues' - The default values for the execution parameter.
--
-- 'type'', 'executionParameter_type' - The execution parameter type.
newExecutionParameter ::
  ExecutionParameter
newExecutionParameter =
  ExecutionParameter'
    { name = Core.Nothing,
      defaultValues = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the execution parameter.
executionParameter_name :: Lens.Lens' ExecutionParameter (Core.Maybe Core.Text)
executionParameter_name = Lens.lens (\ExecutionParameter' {name} -> name) (\s@ExecutionParameter' {} a -> s {name = a} :: ExecutionParameter)

-- | The default values for the execution parameter.
executionParameter_defaultValues :: Lens.Lens' ExecutionParameter (Core.Maybe [Core.Text])
executionParameter_defaultValues = Lens.lens (\ExecutionParameter' {defaultValues} -> defaultValues) (\s@ExecutionParameter' {} a -> s {defaultValues = a} :: ExecutionParameter) Core.. Lens.mapping Lens._Coerce

-- | The execution parameter type.
executionParameter_type :: Lens.Lens' ExecutionParameter (Core.Maybe Core.Text)
executionParameter_type = Lens.lens (\ExecutionParameter' {type'} -> type') (\s@ExecutionParameter' {} a -> s {type' = a} :: ExecutionParameter)

instance Core.FromJSON ExecutionParameter where
  parseJSON =
    Core.withObject
      "ExecutionParameter"
      ( \x ->
          ExecutionParameter'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..:? "DefaultValues" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ExecutionParameter

instance Core.NFData ExecutionParameter
