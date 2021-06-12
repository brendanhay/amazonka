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
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey

-- | Filters for the association execution.
--
-- /See:/ 'newAssociationExecutionTargetsFilter' smart constructor.
data AssociationExecutionTargetsFilter = AssociationExecutionTargetsFilter'
  { -- | The key value used in the request.
    key :: AssociationExecutionTargetsFilterKey,
    -- | The value specified for the key.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationExecutionTargetsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'associationExecutionTargetsFilter_key' - The key value used in the request.
--
-- 'value', 'associationExecutionTargetsFilter_value' - The value specified for the key.
newAssociationExecutionTargetsFilter ::
  -- | 'key'
  AssociationExecutionTargetsFilterKey ->
  -- | 'value'
  Core.Text ->
  AssociationExecutionTargetsFilter
newAssociationExecutionTargetsFilter pKey_ pValue_ =
  AssociationExecutionTargetsFilter'
    { key = pKey_,
      value = pValue_
    }

-- | The key value used in the request.
associationExecutionTargetsFilter_key :: Lens.Lens' AssociationExecutionTargetsFilter AssociationExecutionTargetsFilterKey
associationExecutionTargetsFilter_key = Lens.lens (\AssociationExecutionTargetsFilter' {key} -> key) (\s@AssociationExecutionTargetsFilter' {} a -> s {key = a} :: AssociationExecutionTargetsFilter)

-- | The value specified for the key.
associationExecutionTargetsFilter_value :: Lens.Lens' AssociationExecutionTargetsFilter Core.Text
associationExecutionTargetsFilter_value = Lens.lens (\AssociationExecutionTargetsFilter' {value} -> value) (\s@AssociationExecutionTargetsFilter' {} a -> s {value = a} :: AssociationExecutionTargetsFilter)

instance
  Core.Hashable
    AssociationExecutionTargetsFilter

instance
  Core.NFData
    AssociationExecutionTargetsFilter

instance
  Core.ToJSON
    AssociationExecutionTargetsFilter
  where
  toJSON AssociationExecutionTargetsFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Value" Core..= value)
          ]
      )
