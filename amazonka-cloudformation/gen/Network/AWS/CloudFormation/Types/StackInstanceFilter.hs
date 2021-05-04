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
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceFilter where

import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status that stack instances are filtered by.
--
-- /See:/ 'newStackInstanceFilter' smart constructor.
data StackInstanceFilter = StackInstanceFilter'
  { -- | The status to filter by.
    values :: Prelude.Maybe Prelude.Text,
    -- | The type of filter to apply.
    name :: Prelude.Maybe StackInstanceFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackInstanceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'stackInstanceFilter_values' - The status to filter by.
--
-- 'name', 'stackInstanceFilter_name' - The type of filter to apply.
newStackInstanceFilter ::
  StackInstanceFilter
newStackInstanceFilter =
  StackInstanceFilter'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The status to filter by.
stackInstanceFilter_values :: Lens.Lens' StackInstanceFilter (Prelude.Maybe Prelude.Text)
stackInstanceFilter_values = Lens.lens (\StackInstanceFilter' {values} -> values) (\s@StackInstanceFilter' {} a -> s {values = a} :: StackInstanceFilter)

-- | The type of filter to apply.
stackInstanceFilter_name :: Lens.Lens' StackInstanceFilter (Prelude.Maybe StackInstanceFilterName)
stackInstanceFilter_name = Lens.lens (\StackInstanceFilter' {name} -> name) (\s@StackInstanceFilter' {} a -> s {name = a} :: StackInstanceFilter)

instance Prelude.Hashable StackInstanceFilter

instance Prelude.NFData StackInstanceFilter

instance Prelude.ToQuery StackInstanceFilter where
  toQuery StackInstanceFilter' {..} =
    Prelude.mconcat
      ["Values" Prelude.=: values, "Name" Prelude.=: name]
