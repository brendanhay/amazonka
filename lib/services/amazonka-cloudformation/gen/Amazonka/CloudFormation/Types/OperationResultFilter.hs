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
-- Module      : Amazonka.CloudFormation.Types.OperationResultFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.OperationResultFilter where

import Amazonka.CloudFormation.Types.OperationResultFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status that operation results are filtered by.
--
-- /See:/ 'newOperationResultFilter' smart constructor.
data OperationResultFilter = OperationResultFilter'
  { -- | The type of filter to apply.
    name :: Prelude.Maybe OperationResultFilterName,
    -- | The value to filter by.
    values :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OperationResultFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'operationResultFilter_name' - The type of filter to apply.
--
-- 'values', 'operationResultFilter_values' - The value to filter by.
newOperationResultFilter ::
  OperationResultFilter
newOperationResultFilter =
  OperationResultFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The type of filter to apply.
operationResultFilter_name :: Lens.Lens' OperationResultFilter (Prelude.Maybe OperationResultFilterName)
operationResultFilter_name = Lens.lens (\OperationResultFilter' {name} -> name) (\s@OperationResultFilter' {} a -> s {name = a} :: OperationResultFilter)

-- | The value to filter by.
operationResultFilter_values :: Lens.Lens' OperationResultFilter (Prelude.Maybe Prelude.Text)
operationResultFilter_values = Lens.lens (\OperationResultFilter' {values} -> values) (\s@OperationResultFilter' {} a -> s {values = a} :: OperationResultFilter)

instance Prelude.Hashable OperationResultFilter where
  hashWithSalt _salt OperationResultFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData OperationResultFilter where
  rnf OperationResultFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToQuery OperationResultFilter where
  toQuery OperationResultFilter' {..} =
    Prelude.mconcat
      ["Name" Data.=: name, "Values" Data.=: values]
