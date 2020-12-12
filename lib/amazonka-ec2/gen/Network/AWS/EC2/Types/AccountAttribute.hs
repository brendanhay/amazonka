{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttribute
  ( AccountAttribute (..),

    -- * Smart constructor
    mkAccountAttribute,

    -- * Lenses
    aaAttributeValues,
    aaAttributeName,
  )
where

import Network.AWS.EC2.Types.AccountAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an account attribute.
--
-- /See:/ 'mkAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { attributeValues ::
      Lude.Maybe [AccountAttributeValue],
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

-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the account attribute.
-- * 'attributeValues' - The values for the account attribute.
mkAccountAttribute ::
  AccountAttribute
mkAccountAttribute =
  AccountAttribute'
    { attributeValues = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The values for the account attribute.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeValues :: Lens.Lens' AccountAttribute (Lude.Maybe [AccountAttributeValue])
aaAttributeValues = Lens.lens (attributeValues :: AccountAttribute -> Lude.Maybe [AccountAttributeValue]) (\s a -> s {attributeValues = a} :: AccountAttribute)
{-# DEPRECATED aaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The name of the account attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeName :: Lens.Lens' AccountAttribute (Lude.Maybe Lude.Text)
aaAttributeName = Lens.lens (attributeName :: AccountAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: AccountAttribute)
{-# DEPRECATED aaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Lude.<$> ( x Lude..@? "attributeValueSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "attributeName")
