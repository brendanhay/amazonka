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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowTemplateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowTemplateFilter where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.FlowTemplateFilterName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that filters a workflow search.
--
-- /See:/ 'newFlowTemplateFilter' smart constructor.
data FlowTemplateFilter = FlowTemplateFilter'
  { -- | The name of the search filter field.
    name :: FlowTemplateFilterName,
    -- | An array of string values for the search filter field. Multiple values
    -- function as AND criteria in the search.
    value :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowTemplateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'flowTemplateFilter_name' - The name of the search filter field.
--
-- 'value', 'flowTemplateFilter_value' - An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
newFlowTemplateFilter ::
  -- | 'name'
  FlowTemplateFilterName ->
  FlowTemplateFilter
newFlowTemplateFilter pName_ =
  FlowTemplateFilter'
    { name = pName_,
      value = Prelude.mempty
    }

-- | The name of the search filter field.
flowTemplateFilter_name :: Lens.Lens' FlowTemplateFilter FlowTemplateFilterName
flowTemplateFilter_name = Lens.lens (\FlowTemplateFilter' {name} -> name) (\s@FlowTemplateFilter' {} a -> s {name = a} :: FlowTemplateFilter)

-- | An array of string values for the search filter field. Multiple values
-- function as AND criteria in the search.
flowTemplateFilter_value :: Lens.Lens' FlowTemplateFilter [Prelude.Text]
flowTemplateFilter_value = Lens.lens (\FlowTemplateFilter' {value} -> value) (\s@FlowTemplateFilter' {} a -> s {value = a} :: FlowTemplateFilter) Prelude.. Lens.coerced

instance Prelude.Hashable FlowTemplateFilter where
  hashWithSalt salt' FlowTemplateFilter' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData FlowTemplateFilter where
  rnf FlowTemplateFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Core.ToJSON FlowTemplateFilter where
  toJSON FlowTemplateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("value" Core..= value)
          ]
      )
