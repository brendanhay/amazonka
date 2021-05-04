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
-- Module      : Network.AWS.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.InstanceInformationFilterKey

-- | Describes a filter for a specific list of instances. You can filter
-- instances information by using tags. You specify tags by using a
-- key-value mapping.
--
-- Use this action instead of the
-- DescribeInstanceInformationRequest$InstanceInformationFilterList method.
-- The @InstanceInformationFilterList@ method is a legacy method and does
-- not support tags.
--
-- /See:/ 'newInstanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { -- | The name of the filter.
    key :: InstanceInformationFilterKey,
    -- | The filter values.
    valueSet :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceInformationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instanceInformationFilter_key' - The name of the filter.
--
-- 'valueSet', 'instanceInformationFilter_valueSet' - The filter values.
newInstanceInformationFilter ::
  -- | 'key'
  InstanceInformationFilterKey ->
  -- | 'valueSet'
  Prelude.NonEmpty Prelude.Text ->
  InstanceInformationFilter
newInstanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter'
    { key = pKey_,
      valueSet = Prelude._Coerce Lens.# pValueSet_
    }

-- | The name of the filter.
instanceInformationFilter_key :: Lens.Lens' InstanceInformationFilter InstanceInformationFilterKey
instanceInformationFilter_key = Lens.lens (\InstanceInformationFilter' {key} -> key) (\s@InstanceInformationFilter' {} a -> s {key = a} :: InstanceInformationFilter)

-- | The filter values.
instanceInformationFilter_valueSet :: Lens.Lens' InstanceInformationFilter (Prelude.NonEmpty Prelude.Text)
instanceInformationFilter_valueSet = Lens.lens (\InstanceInformationFilter' {valueSet} -> valueSet) (\s@InstanceInformationFilter' {} a -> s {valueSet = a} :: InstanceInformationFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable InstanceInformationFilter

instance Prelude.NFData InstanceInformationFilter

instance Prelude.ToJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("valueSet" Prelude..= valueSet)
          ]
      )
