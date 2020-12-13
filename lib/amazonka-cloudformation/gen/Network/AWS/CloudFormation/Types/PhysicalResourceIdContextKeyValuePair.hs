{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
  ( PhysicalResourceIdContextKeyValuePair (..),

    -- * Smart constructor
    mkPhysicalResourceIdContextKeyValuePair,

    -- * Lenses
    prickvpValue,
    prickvpKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a resource that contains the targeted resource.
--
-- /See:/ 'mkPhysicalResourceIdContextKeyValuePair' smart constructor.
data PhysicalResourceIdContextKeyValuePair = PhysicalResourceIdContextKeyValuePair'
  { -- | The resource context value.
    value :: Lude.Text,
    -- | The resource context key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhysicalResourceIdContextKeyValuePair' with the minimum fields required to make a request.
--
-- * 'value' - The resource context value.
-- * 'key' - The resource context key.
mkPhysicalResourceIdContextKeyValuePair ::
  -- | 'value'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  PhysicalResourceIdContextKeyValuePair
mkPhysicalResourceIdContextKeyValuePair pValue_ pKey_ =
  PhysicalResourceIdContextKeyValuePair'
    { value = pValue_,
      key = pKey_
    }

-- | The resource context value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prickvpValue :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Lude.Text
prickvpValue = Lens.lens (value :: PhysicalResourceIdContextKeyValuePair -> Lude.Text) (\s a -> s {value = a} :: PhysicalResourceIdContextKeyValuePair)
{-# DEPRECATED prickvpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The resource context key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prickvpKey :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Lude.Text
prickvpKey = Lens.lens (key :: PhysicalResourceIdContextKeyValuePair -> Lude.Text) (\s a -> s {key = a} :: PhysicalResourceIdContextKeyValuePair)
{-# DEPRECATED prickvpKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML PhysicalResourceIdContextKeyValuePair where
  parseXML x =
    PhysicalResourceIdContextKeyValuePair'
      Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Key")
