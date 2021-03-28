{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.LookupAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.LookupAttribute
  ( LookupAttribute (..)
  -- * Smart constructor
  , mkLookupAttribute
  -- * Lenses
  , laAttributeKey
  , laAttributeValue
  ) where

import qualified Network.AWS.CloudTrail.Types.LookupAttributeKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an attribute and value that filter the events returned.
--
-- /See:/ 'mkLookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
  { attributeKey :: Types.LookupAttributeKey
    -- ^ Specifies an attribute on which to filter the events returned.
  , attributeValue :: Core.Text
    -- ^ Specifies a value for the specified AttributeKey.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LookupAttribute' value with any optional fields omitted.
mkLookupAttribute
    :: Types.LookupAttributeKey -- ^ 'attributeKey'
    -> Core.Text -- ^ 'attributeValue'
    -> LookupAttribute
mkLookupAttribute attributeKey attributeValue
  = LookupAttribute'{attributeKey, attributeValue}

-- | Specifies an attribute on which to filter the events returned.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeKey :: Lens.Lens' LookupAttribute Types.LookupAttributeKey
laAttributeKey = Lens.field @"attributeKey"
{-# INLINEABLE laAttributeKey #-}
{-# DEPRECATED attributeKey "Use generic-lens or generic-optics with 'attributeKey' instead"  #-}

-- | Specifies a value for the specified AttributeKey.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeValue :: Lens.Lens' LookupAttribute Core.Text
laAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE laAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.FromJSON LookupAttribute where
        toJSON LookupAttribute{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AttributeKey" Core..= attributeKey),
                  Core.Just ("AttributeValue" Core..= attributeValue)])
