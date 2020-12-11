-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AttributeDefinition
  ( AttributeDefinition (..),

    -- * Smart constructor
    mkAttributeDefinition,

    -- * Lenses
    adAttributeName,
    adAttributeType,
  )
where

import Network.AWS.DynamoDB.Types.ScalarAttributeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an attribute for describing the key schema for the table and indexes.
--
-- /See:/ 'mkAttributeDefinition' smart constructor.
data AttributeDefinition = AttributeDefinition'
  { attributeName ::
      Lude.Text,
    attributeType :: ScalarAttributeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeDefinition' with the minimum fields required to make a request.
--
-- * 'attributeName' - A name for the attribute.
-- * 'attributeType' - The data type for the attribute, where:
--
--
--     * @S@ - the attribute is of type String
--
--
--     * @N@ - the attribute is of type Number
--
--
--     * @B@ - the attribute is of type Binary
mkAttributeDefinition ::
  -- | 'attributeName'
  Lude.Text ->
  -- | 'attributeType'
  ScalarAttributeType ->
  AttributeDefinition
mkAttributeDefinition pAttributeName_ pAttributeType_ =
  AttributeDefinition'
    { attributeName = pAttributeName_,
      attributeType = pAttributeType_
    }

-- | A name for the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttributeName :: Lens.Lens' AttributeDefinition Lude.Text
adAttributeName = Lens.lens (attributeName :: AttributeDefinition -> Lude.Text) (\s a -> s {attributeName = a} :: AttributeDefinition)
{-# DEPRECATED adAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The data type for the attribute, where:
--
--
--     * @S@ - the attribute is of type String
--
--
--     * @N@ - the attribute is of type Number
--
--
--     * @B@ - the attribute is of type Binary
--
--
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttributeType :: Lens.Lens' AttributeDefinition ScalarAttributeType
adAttributeType = Lens.lens (attributeType :: AttributeDefinition -> ScalarAttributeType) (\s a -> s {attributeType = a} :: AttributeDefinition)
{-# DEPRECATED adAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

instance Lude.FromJSON AttributeDefinition where
  parseJSON =
    Lude.withObject
      "AttributeDefinition"
      ( \x ->
          AttributeDefinition'
            Lude.<$> (x Lude..: "AttributeName") Lude.<*> (x Lude..: "AttributeType")
      )

instance Lude.ToJSON AttributeDefinition where
  toJSON AttributeDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeName" Lude..= attributeName),
            Lude.Just ("AttributeType" Lude..= attributeType)
          ]
      )
