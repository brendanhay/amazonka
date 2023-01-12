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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.PropertyFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.PropertyFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.Property

-- |
--
-- /See:/ 'newPropertyFilter' smart constructor.
data PropertyFilter = PropertyFilter'
  { property :: Property
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'property', 'propertyFilter_property' -
newPropertyFilter ::
  -- | 'property'
  Property ->
  PropertyFilter
newPropertyFilter pProperty_ =
  PropertyFilter' {property = pProperty_}

-- |
propertyFilter_property :: Lens.Lens' PropertyFilter Property
propertyFilter_property = Lens.lens (\PropertyFilter' {property} -> property) (\s@PropertyFilter' {} a -> s {property = a} :: PropertyFilter)

instance Data.FromJSON PropertyFilter where
  parseJSON =
    Data.withObject
      "PropertyFilter"
      ( \x ->
          PropertyFilter' Prelude.<$> (x Data..: "Property")
      )

instance Prelude.Hashable PropertyFilter where
  hashWithSalt _salt PropertyFilter' {..} =
    _salt `Prelude.hashWithSalt` property

instance Prelude.NFData PropertyFilter where
  rnf PropertyFilter' {..} = Prelude.rnf property

instance Data.ToJSON PropertyFilter where
  toJSON PropertyFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Property" Data..= property)]
      )
