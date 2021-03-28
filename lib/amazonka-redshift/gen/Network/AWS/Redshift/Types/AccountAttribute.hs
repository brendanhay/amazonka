{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.AccountAttribute
  ( AccountAttribute (..)
  -- * Smart constructor
  , mkAccountAttribute
  -- * Lenses
  , aaAttributeName
  , aaAttributeValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AttributeValueTarget as Types

-- | A name value pair that describes an aspect of an account. 
--
-- /See:/ 'mkAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { attributeName :: Core.Maybe Core.Text
    -- ^ The name of the attribute.
  , attributeValues :: Core.Maybe [Types.AttributeValueTarget]
    -- ^ A list of attribute values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountAttribute' value with any optional fields omitted.
mkAccountAttribute
    :: AccountAttribute
mkAccountAttribute
  = AccountAttribute'{attributeName = Core.Nothing,
                      attributeValues = Core.Nothing}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeName :: Lens.Lens' AccountAttribute (Core.Maybe Core.Text)
aaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE aaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | A list of attribute values.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeValues :: Lens.Lens' AccountAttribute (Core.Maybe [Types.AttributeValueTarget])
aaAttributeValues = Lens.field @"attributeValues"
{-# INLINEABLE aaAttributeValues #-}
{-# DEPRECATED attributeValues "Use generic-lens or generic-optics with 'attributeValues' instead"  #-}

instance Core.FromXML AccountAttribute where
        parseXML x
          = AccountAttribute' Core.<$>
              (x Core..@? "AttributeName") Core.<*>
                x Core..@? "AttributeValues" Core..<@>
                  Core.parseXMLList "AttributeValueTarget"
