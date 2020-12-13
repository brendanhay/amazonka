{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.AttributeValue
  ( AttributeValue (..),

    -- * Smart constructor
    mkAttributeValue,

    -- * Lenses
    avValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
--
-- /See:/ 'mkAttributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { -- | The specific value of an @attributeName@ .
    value :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- * 'value' - The specific value of an @attributeName@ .
mkAttributeValue ::
  AttributeValue
mkAttributeValue = AttributeValue' {value = Lude.Nothing}

-- | The specific value of an @attributeName@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avValue :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Text)
avValue = Lens.lens (value :: AttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: AttributeValue)
{-# DEPRECATED avValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON AttributeValue where
  parseJSON =
    Lude.withObject
      "AttributeValue"
      (\x -> AttributeValue' Lude.<$> (x Lude..:? "Value"))
