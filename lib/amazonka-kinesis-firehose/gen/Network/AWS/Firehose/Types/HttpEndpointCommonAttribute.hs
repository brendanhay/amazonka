{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
  ( HttpEndpointCommonAttribute (..)
  -- * Smart constructor
  , mkHttpEndpointCommonAttribute
  -- * Lenses
  , hecaAttributeName
  , hecaAttributeValue
  ) where

import qualified Network.AWS.Firehose.Types.HttpEndpointAttributeName as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the metadata that's delivered to the specified HTTP endpoint destination.
--
-- /See:/ 'mkHttpEndpointCommonAttribute' smart constructor.
data HttpEndpointCommonAttribute = HttpEndpointCommonAttribute'
  { attributeName :: Types.HttpEndpointAttributeName
    -- ^ The name of the HTTP endpoint common attribute.
  , attributeValue :: Types.HttpEndpointAttributeValue
    -- ^ The value of the HTTP endpoint common attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointCommonAttribute' value with any optional fields omitted.
mkHttpEndpointCommonAttribute
    :: Types.HttpEndpointAttributeName -- ^ 'attributeName'
    -> Types.HttpEndpointAttributeValue -- ^ 'attributeValue'
    -> HttpEndpointCommonAttribute
mkHttpEndpointCommonAttribute attributeName attributeValue
  = HttpEndpointCommonAttribute'{attributeName, attributeValue}

-- | The name of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hecaAttributeName :: Lens.Lens' HttpEndpointCommonAttribute Types.HttpEndpointAttributeName
hecaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE hecaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hecaAttributeValue :: Lens.Lens' HttpEndpointCommonAttribute Types.HttpEndpointAttributeValue
hecaAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE hecaAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.FromJSON HttpEndpointCommonAttribute where
        toJSON HttpEndpointCommonAttribute{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AttributeName" Core..= attributeName),
                  Core.Just ("AttributeValue" Core..= attributeValue)])

instance Core.FromJSON HttpEndpointCommonAttribute where
        parseJSON
          = Core.withObject "HttpEndpointCommonAttribute" Core.$
              \ x ->
                HttpEndpointCommonAttribute' Core.<$>
                  (x Core..: "AttributeName") Core.<*> x Core..: "AttributeValue"
