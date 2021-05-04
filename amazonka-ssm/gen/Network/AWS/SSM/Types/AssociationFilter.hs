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
-- Module      : Network.AWS.SSM.Types.AssociationFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.AssociationFilterKey

-- | Describes a filter.
--
-- /See:/ 'newAssociationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
  { -- | The name of the filter.
    --
    -- @InstanceId@ has been deprecated.
    key :: AssociationFilterKey,
    -- | The filter value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'associationFilter_key' - The name of the filter.
--
-- @InstanceId@ has been deprecated.
--
-- 'value', 'associationFilter_value' - The filter value.
newAssociationFilter ::
  -- | 'key'
  AssociationFilterKey ->
  -- | 'value'
  Prelude.Text ->
  AssociationFilter
newAssociationFilter pKey_ pValue_ =
  AssociationFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
--
-- @InstanceId@ has been deprecated.
associationFilter_key :: Lens.Lens' AssociationFilter AssociationFilterKey
associationFilter_key = Lens.lens (\AssociationFilter' {key} -> key) (\s@AssociationFilter' {} a -> s {key = a} :: AssociationFilter)

-- | The filter value.
associationFilter_value :: Lens.Lens' AssociationFilter Prelude.Text
associationFilter_value = Lens.lens (\AssociationFilter' {value} -> value) (\s@AssociationFilter' {} a -> s {value = a} :: AssociationFilter)

instance Prelude.Hashable AssociationFilter

instance Prelude.NFData AssociationFilter

instance Prelude.ToJSON AssociationFilter where
  toJSON AssociationFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
