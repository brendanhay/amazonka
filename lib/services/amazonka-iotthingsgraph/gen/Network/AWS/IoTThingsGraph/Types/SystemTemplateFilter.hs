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
-- Module      : Network.AWS.IoTThingsGraph.Types.SystemTemplateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTThingsGraph.Types.SystemTemplateFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types.SystemTemplateFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that filters a system search.
--
-- /See:/ 'newSystemTemplateFilter' smart constructor.
data SystemTemplateFilter = SystemTemplateFilter'
  { -- | The name of the system search filter field.
    name :: SystemTemplateFilterName,
    -- | An array of string values for the search filter field. Multiple values
    -- function as AND criteria in the search.
    value :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemTemplateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'systemTemplateFilter_name' - The name of the system search filter field.
--
-- 'value', 'systemTemplateFilter_value' - An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
newSystemTemplateFilter ::
  -- | 'name'
  SystemTemplateFilterName ->
  SystemTemplateFilter
newSystemTemplateFilter pName_ =
  SystemTemplateFilter'
    { name = pName_,
      value = Prelude.mempty
    }

-- | The name of the system search filter field.
systemTemplateFilter_name :: Lens.Lens' SystemTemplateFilter SystemTemplateFilterName
systemTemplateFilter_name = Lens.lens (\SystemTemplateFilter' {name} -> name) (\s@SystemTemplateFilter' {} a -> s {name = a} :: SystemTemplateFilter)

-- | An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
systemTemplateFilter_value :: Lens.Lens' SystemTemplateFilter [Prelude.Text]
systemTemplateFilter_value = Lens.lens (\SystemTemplateFilter' {value} -> value) (\s@SystemTemplateFilter' {} a -> s {value = a} :: SystemTemplateFilter) Prelude.. Lens.coerced

instance Prelude.Hashable SystemTemplateFilter

instance Prelude.NFData SystemTemplateFilter

instance Core.ToJSON SystemTemplateFilter where
  toJSON SystemTemplateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("value" Core..= value)
          ]
      )
