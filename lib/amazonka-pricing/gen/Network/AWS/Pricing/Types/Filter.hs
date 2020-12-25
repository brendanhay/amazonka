{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fType,
    fField,
    fValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Pricing.Types.FilterType as Types
import qualified Network.AWS.Pricing.Types.String as Types

-- | The constraints that you want all returned products to match.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The type of filter that you want to use.
    --
    -- Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
    type' :: Types.FilterType,
    -- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields.
    --
    -- Valid values include: @ServiceCode@ , and all attribute names
    -- For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
    field :: Types.String,
    -- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
    value :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  -- | 'type\''
  Types.FilterType ->
  -- | 'field'
  Types.String ->
  -- | 'value'
  Types.String ->
  Filter
mkFilter type' field value = Filter' {type', field, value}

-- | The type of filter that you want to use.
--
-- Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Filter Types.FilterType
fType = Lens.field @"type'"
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields.
--
-- Valid values include: @ServiceCode@ , and all attribute names
-- For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fField :: Lens.Lens' Filter Types.String
fField = Lens.field @"field"
{-# DEPRECATED fField "Use generic-lens or generic-optics with 'field' instead." #-}

-- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValue :: Lens.Lens' Filter Types.String
fValue = Lens.field @"value"
{-# DEPRECATED fValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Filter where
  toJSON Filter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Field" Core..= field),
            Core.Just ("Value" Core..= value)
          ]
      )
