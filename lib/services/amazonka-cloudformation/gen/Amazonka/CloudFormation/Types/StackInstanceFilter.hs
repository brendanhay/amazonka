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
-- Module      : Amazonka.CloudFormation.Types.StackInstanceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackInstanceFilter where

import Amazonka.CloudFormation.Types.StackInstanceFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The filter to apply to stack instances
--
-- /See:/ 'newStackInstanceFilter' smart constructor.
data StackInstanceFilter = StackInstanceFilter'
  { -- | The type of filter to apply.
    name :: Prelude.Maybe StackInstanceFilterName,
    -- | The status to filter by.
    values :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackInstanceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stackInstanceFilter_name' - The type of filter to apply.
--
-- 'values', 'stackInstanceFilter_values' - The status to filter by.
newStackInstanceFilter ::
  StackInstanceFilter
newStackInstanceFilter =
  StackInstanceFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The type of filter to apply.
stackInstanceFilter_name :: Lens.Lens' StackInstanceFilter (Prelude.Maybe StackInstanceFilterName)
stackInstanceFilter_name = Lens.lens (\StackInstanceFilter' {name} -> name) (\s@StackInstanceFilter' {} a -> s {name = a} :: StackInstanceFilter)

-- | The status to filter by.
stackInstanceFilter_values :: Lens.Lens' StackInstanceFilter (Prelude.Maybe Prelude.Text)
stackInstanceFilter_values = Lens.lens (\StackInstanceFilter' {values} -> values) (\s@StackInstanceFilter' {} a -> s {values = a} :: StackInstanceFilter)

instance Prelude.Hashable StackInstanceFilter where
  hashWithSalt _salt StackInstanceFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData StackInstanceFilter where
  rnf StackInstanceFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToQuery StackInstanceFilter where
  toQuery StackInstanceFilter' {..} =
    Prelude.mconcat
      ["Name" Data.=: name, "Values" Data.=: values]
