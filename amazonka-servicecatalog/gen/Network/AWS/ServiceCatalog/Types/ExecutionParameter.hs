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
-- Module      : Network.AWS.ServiceCatalog.Types.ExecutionParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ExecutionParameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of an execution parameter value that is passed to a self-service
-- action when executed on a provisioned product.
--
-- /See:/ 'newExecutionParameter' smart constructor.
data ExecutionParameter = ExecutionParameter'
  { -- | The name of the execution parameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The default values for the execution parameter.
    defaultValues :: Prelude.Maybe [Prelude.Text],
    -- | The execution parameter type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { name = Prelude.Nothing,
      defaultValues = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the execution parameter.
executionParameter_name :: Lens.Lens' ExecutionParameter (Prelude.Maybe Prelude.Text)
executionParameter_name = Lens.lens (\ExecutionParameter' {name} -> name) (\s@ExecutionParameter' {} a -> s {name = a} :: ExecutionParameter)

-- | The default values for the execution parameter.
executionParameter_defaultValues :: Lens.Lens' ExecutionParameter (Prelude.Maybe [Prelude.Text])
executionParameter_defaultValues = Lens.lens (\ExecutionParameter' {defaultValues} -> defaultValues) (\s@ExecutionParameter' {} a -> s {defaultValues = a} :: ExecutionParameter) Prelude.. Lens.mapping Prelude._Coerce

-- | The execution parameter type.
executionParameter_type :: Lens.Lens' ExecutionParameter (Prelude.Maybe Prelude.Text)
executionParameter_type = Lens.lens (\ExecutionParameter' {type'} -> type') (\s@ExecutionParameter' {} a -> s {type' = a} :: ExecutionParameter)

instance Prelude.FromJSON ExecutionParameter where
  parseJSON =
    Prelude.withObject
      "ExecutionParameter"
      ( \x ->
          ExecutionParameter'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> ( x Prelude..:? "DefaultValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ExecutionParameter

instance Prelude.NFData ExecutionParameter
