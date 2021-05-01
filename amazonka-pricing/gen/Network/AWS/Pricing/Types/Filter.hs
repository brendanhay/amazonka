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
-- Module      : Network.AWS.Pricing.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.Filter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Pricing.Types.FilterType

-- | The constraints that you want all returned products to match.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The type of filter that you want to use.
    --
    -- Valid values are: @TERM_MATCH@. @TERM_MATCH@ returns only products that
    -- match both the given filter field and the given value.
    type' :: FilterType,
    -- | The product metadata field that you want to filter on. You can filter by
    -- just the service code to see all products for a specific service, filter
    -- by just the attribute name to see a specific attribute for multiple
    -- services, or use both a service code and an attribute name to retrieve
    -- only products that match both fields.
    --
    -- Valid values include: @ServiceCode@, and all attribute names
    --
    -- For example, you can filter by the @AmazonEC2@ service code and the
    -- @volumeType@ attribute name to get the prices for only Amazon EC2
    -- volumes.
    field :: Prelude.Text,
    -- | The service code or attribute value that you want to filter by. If you
    -- are filtering by service code this is the actual service code, such as
    -- @AmazonEC2@. If you are filtering by attribute name, this is the
    -- attribute value that you want the returned products to match, such as a
    -- @Provisioned IOPS@ volume.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'filter_type' - The type of filter that you want to use.
--
-- Valid values are: @TERM_MATCH@. @TERM_MATCH@ returns only products that
-- match both the given filter field and the given value.
--
-- 'field', 'filter_field' - The product metadata field that you want to filter on. You can filter by
-- just the service code to see all products for a specific service, filter
-- by just the attribute name to see a specific attribute for multiple
-- services, or use both a service code and an attribute name to retrieve
-- only products that match both fields.
--
-- Valid values include: @ServiceCode@, and all attribute names
--
-- For example, you can filter by the @AmazonEC2@ service code and the
-- @volumeType@ attribute name to get the prices for only Amazon EC2
-- volumes.
--
-- 'value', 'filter_value' - The service code or attribute value that you want to filter by. If you
-- are filtering by service code this is the actual service code, such as
-- @AmazonEC2@. If you are filtering by attribute name, this is the
-- attribute value that you want the returned products to match, such as a
-- @Provisioned IOPS@ volume.
newFilter ::
  -- | 'type''
  FilterType ->
  -- | 'field'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Filter
newFilter pType_ pField_ pValue_ =
  Filter'
    { type' = pType_,
      field = pField_,
      value = pValue_
    }

-- | The type of filter that you want to use.
--
-- Valid values are: @TERM_MATCH@. @TERM_MATCH@ returns only products that
-- match both the given filter field and the given value.
filter_type :: Lens.Lens' Filter FilterType
filter_type = Lens.lens (\Filter' {type'} -> type') (\s@Filter' {} a -> s {type' = a} :: Filter)

-- | The product metadata field that you want to filter on. You can filter by
-- just the service code to see all products for a specific service, filter
-- by just the attribute name to see a specific attribute for multiple
-- services, or use both a service code and an attribute name to retrieve
-- only products that match both fields.
--
-- Valid values include: @ServiceCode@, and all attribute names
--
-- For example, you can filter by the @AmazonEC2@ service code and the
-- @volumeType@ attribute name to get the prices for only Amazon EC2
-- volumes.
filter_field :: Lens.Lens' Filter Prelude.Text
filter_field = Lens.lens (\Filter' {field} -> field) (\s@Filter' {} a -> s {field = a} :: Filter)

-- | The service code or attribute value that you want to filter by. If you
-- are filtering by service code this is the actual service code, such as
-- @AmazonEC2@. If you are filtering by attribute name, this is the
-- attribute value that you want the returned products to match, such as a
-- @Provisioned IOPS@ volume.
filter_value :: Lens.Lens' Filter Prelude.Text
filter_value = Lens.lens (\Filter' {value} -> value) (\s@Filter' {} a -> s {value = a} :: Filter)

instance Prelude.Hashable Filter

instance Prelude.NFData Filter

instance Prelude.ToJSON Filter where
  toJSON Filter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("Field" Prelude..= field),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
