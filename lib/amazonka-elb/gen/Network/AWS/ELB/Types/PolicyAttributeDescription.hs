{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeDescription
  ( PolicyAttributeDescription (..),

    -- * Smart constructor
    mkPolicyAttributeDescription,

    -- * Lenses
    padAttributeValue,
    padAttributeName,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy attribute.
--
-- /See:/ 'mkPolicyAttributeDescription' smart constructor.
data PolicyAttributeDescription = PolicyAttributeDescription'
  { -- | The value of the attribute.
    attributeValue :: Lude.Maybe Lude.Text,
    -- | The name of the attribute.
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyAttributeDescription' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value of the attribute.
-- * 'attributeName' - The name of the attribute.
mkPolicyAttributeDescription ::
  PolicyAttributeDescription
mkPolicyAttributeDescription =
  PolicyAttributeDescription'
    { attributeValue = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padAttributeValue :: Lens.Lens' PolicyAttributeDescription (Lude.Maybe Lude.Text)
padAttributeValue = Lens.lens (attributeValue :: PolicyAttributeDescription -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: PolicyAttributeDescription)
{-# DEPRECATED padAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padAttributeName :: Lens.Lens' PolicyAttributeDescription (Lude.Maybe Lude.Text)
padAttributeName = Lens.lens (attributeName :: PolicyAttributeDescription -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: PolicyAttributeDescription)
{-# DEPRECATED padAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromXML PolicyAttributeDescription where
  parseXML x =
    PolicyAttributeDescription'
      Lude.<$> (x Lude..@? "AttributeValue") Lude.<*> (x Lude..@? "AttributeName")
