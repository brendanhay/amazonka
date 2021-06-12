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
-- Module      : Network.AWS.SSM.Types.AssociationExecutionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AssociationExecutionFilterKey
import Network.AWS.SSM.Types.AssociationFilterOperatorType

-- | Filters used in the request.
--
-- /See:/ 'newAssociationExecutionFilter' smart constructor.
data AssociationExecutionFilter = AssociationExecutionFilter'
  { -- | The key value used in the request.
    key :: AssociationExecutionFilterKey,
    -- | The value specified for the key.
    value :: Core.Text,
    -- | The filter type specified in the request.
    type' :: AssociationFilterOperatorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationExecutionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'associationExecutionFilter_key' - The key value used in the request.
--
-- 'value', 'associationExecutionFilter_value' - The value specified for the key.
--
-- 'type'', 'associationExecutionFilter_type' - The filter type specified in the request.
newAssociationExecutionFilter ::
  -- | 'key'
  AssociationExecutionFilterKey ->
  -- | 'value'
  Core.Text ->
  -- | 'type''
  AssociationFilterOperatorType ->
  AssociationExecutionFilter
newAssociationExecutionFilter pKey_ pValue_ pType_ =
  AssociationExecutionFilter'
    { key = pKey_,
      value = pValue_,
      type' = pType_
    }

-- | The key value used in the request.
associationExecutionFilter_key :: Lens.Lens' AssociationExecutionFilter AssociationExecutionFilterKey
associationExecutionFilter_key = Lens.lens (\AssociationExecutionFilter' {key} -> key) (\s@AssociationExecutionFilter' {} a -> s {key = a} :: AssociationExecutionFilter)

-- | The value specified for the key.
associationExecutionFilter_value :: Lens.Lens' AssociationExecutionFilter Core.Text
associationExecutionFilter_value = Lens.lens (\AssociationExecutionFilter' {value} -> value) (\s@AssociationExecutionFilter' {} a -> s {value = a} :: AssociationExecutionFilter)

-- | The filter type specified in the request.
associationExecutionFilter_type :: Lens.Lens' AssociationExecutionFilter AssociationFilterOperatorType
associationExecutionFilter_type = Lens.lens (\AssociationExecutionFilter' {type'} -> type') (\s@AssociationExecutionFilter' {} a -> s {type' = a} :: AssociationExecutionFilter)

instance Core.Hashable AssociationExecutionFilter

instance Core.NFData AssociationExecutionFilter

instance Core.ToJSON AssociationExecutionFilter where
  toJSON AssociationExecutionFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Value" Core..= value),
            Core.Just ("Type" Core..= type')
          ]
      )
