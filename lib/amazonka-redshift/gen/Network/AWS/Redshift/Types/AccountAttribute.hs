-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountAttribute
  ( AccountAttribute (..),

    -- * Smart constructor
    mkAccountAttribute,

    -- * Lenses
    aaAttributeValues,
    aaAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AttributeValueTarget

-- | A name value pair that describes an aspect of an account.
--
-- /See:/ 'mkAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { attributeValues ::
      Lude.Maybe [AttributeValueTarget],
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
-- * 'attributeName' - The name of the attribute.
-- * 'attributeValues' - A list of attribute values.
mkAccountAttribute ::
  AccountAttribute
mkAccountAttribute =
  AccountAttribute'
    { attributeValues = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | A list of attribute values.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeValues :: Lens.Lens' AccountAttribute (Lude.Maybe [AttributeValueTarget])
aaAttributeValues = Lens.lens (attributeValues :: AccountAttribute -> Lude.Maybe [AttributeValueTarget]) (\s a -> s {attributeValues = a} :: AccountAttribute)
{-# DEPRECATED aaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeName :: Lens.Lens' AccountAttribute (Lude.Maybe Lude.Text)
aaAttributeName = Lens.lens (attributeName :: AccountAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: AccountAttribute)
{-# DEPRECATED aaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Lude.<$> ( x Lude..@? "AttributeValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AttributeValueTarget")
               )
      Lude.<*> (x Lude..@? "AttributeName")
