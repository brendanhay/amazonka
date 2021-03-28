{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.AttributeDefinition
  ( AttributeDefinition (..)
  -- * Smart constructor
  , mkAttributeDefinition
  -- * Lenses
  , adAttributeName
  , adAttributeType
  ) where

import qualified Network.AWS.DynamoDB.Types.KeySchemaAttributeName as Types
import qualified Network.AWS.DynamoDB.Types.ScalarAttributeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an attribute for describing the key schema for the table and indexes.
--
-- /See:/ 'mkAttributeDefinition' smart constructor.
data AttributeDefinition = AttributeDefinition'
  { attributeName :: Types.KeySchemaAttributeName
    -- ^ A name for the attribute.
  , attributeType :: Types.ScalarAttributeType
    -- ^ The data type for the attribute, where:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeDefinition' value with any optional fields omitted.
mkAttributeDefinition
    :: Types.KeySchemaAttributeName -- ^ 'attributeName'
    -> Types.ScalarAttributeType -- ^ 'attributeType'
    -> AttributeDefinition
mkAttributeDefinition attributeName attributeType
  = AttributeDefinition'{attributeName, attributeType}

-- | A name for the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttributeName :: Lens.Lens' AttributeDefinition Types.KeySchemaAttributeName
adAttributeName = Lens.field @"attributeName"
{-# INLINEABLE adAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

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
adAttributeType :: Lens.Lens' AttributeDefinition Types.ScalarAttributeType
adAttributeType = Lens.field @"attributeType"
{-# INLINEABLE adAttributeType #-}
{-# DEPRECATED attributeType "Use generic-lens or generic-optics with 'attributeType' instead"  #-}

instance Core.FromJSON AttributeDefinition where
        toJSON AttributeDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AttributeName" Core..= attributeName),
                  Core.Just ("AttributeType" Core..= attributeType)])

instance Core.FromJSON AttributeDefinition where
        parseJSON
          = Core.withObject "AttributeDefinition" Core.$
              \ x ->
                AttributeDefinition' Core.<$>
                  (x Core..: "AttributeName") Core.<*> x Core..: "AttributeType"
