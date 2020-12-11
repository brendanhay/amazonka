-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttribute
  ( PolicyAttribute (..),

    -- * Smart constructor
    mkPolicyAttribute,

    -- * Lenses
    paAttributeValue,
    paAttributeName,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy attribute.
--
-- /See:/ 'mkPolicyAttribute' smart constructor.
data PolicyAttribute = PolicyAttribute'
  { attributeValue ::
      Lude.Maybe Lude.Text,
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the attribute.
-- * 'attributeValue' - The value of the attribute.
mkPolicyAttribute ::
  PolicyAttribute
mkPolicyAttribute =
  PolicyAttribute'
    { attributeValue = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributeValue :: Lens.Lens' PolicyAttribute (Lude.Maybe Lude.Text)
paAttributeValue = Lens.lens (attributeValue :: PolicyAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: PolicyAttribute)
{-# DEPRECATED paAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributeName :: Lens.Lens' PolicyAttribute (Lude.Maybe Lude.Text)
paAttributeName = Lens.lens (attributeName :: PolicyAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: PolicyAttribute)
{-# DEPRECATED paAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.ToQuery PolicyAttribute where
  toQuery PolicyAttribute' {..} =
    Lude.mconcat
      [ "AttributeValue" Lude.=: attributeValue,
        "AttributeName" Lude.=: attributeName
      ]
