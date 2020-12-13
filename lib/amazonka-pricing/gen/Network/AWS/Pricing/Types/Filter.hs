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
    fField,
    fValue,
    fType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Pricing.Types.FilterType

-- | The constraints that you want all returned products to match.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields.
    --
    -- Valid values include: @ServiceCode@ , and all attribute names
    -- For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
    field :: Lude.Text,
    -- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
    value :: Lude.Text,
    -- | The type of filter that you want to use.
    --
    -- Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
    type' :: FilterType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'field' - The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields.
--
-- Valid values include: @ServiceCode@ , and all attribute names
-- For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
-- * 'value' - The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
-- * 'type'' - The type of filter that you want to use.
--
-- Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
mkFilter ::
  -- | 'field'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  -- | 'type''
  FilterType ->
  Filter
mkFilter pField_ pValue_ pType_ =
  Filter' {field = pField_, value = pValue_, type' = pType_}

-- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields.
--
-- Valid values include: @ServiceCode@ , and all attribute names
-- For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fField :: Lens.Lens' Filter Lude.Text
fField = Lens.lens (field :: Filter -> Lude.Text) (\s a -> s {field = a} :: Filter)
{-# DEPRECATED fField "Use generic-lens or generic-optics with 'field' instead." #-}

-- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValue :: Lens.Lens' Filter Lude.Text
fValue = Lens.lens (value :: Filter -> Lude.Text) (\s a -> s {value = a} :: Filter)
{-# DEPRECATED fValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of filter that you want to use.
--
-- Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Filter FilterType
fType = Lens.lens (type' :: Filter -> FilterType) (\s a -> s {type' = a} :: Filter)
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON Filter where
  toJSON Filter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Field" Lude..= field),
            Lude.Just ("Value" Lude..= value),
            Lude.Just ("Type" Lude..= type')
          ]
      )
