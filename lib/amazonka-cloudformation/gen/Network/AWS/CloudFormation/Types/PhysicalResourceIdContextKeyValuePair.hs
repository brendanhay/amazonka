{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
  ( PhysicalResourceIdContextKeyValuePair (..)
  -- * Smart constructor
  , mkPhysicalResourceIdContextKeyValuePair
  -- * Lenses
  , prickvpKey
  , prickvpValue
  ) where

import qualified Network.AWS.CloudFormation.Types.Key as Types
import qualified Network.AWS.CloudFormation.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Context information that enables AWS CloudFormation to uniquely identify a resource. AWS CloudFormation uses context key-value pairs in cases where a resource's logical and physical IDs are not enough to uniquely identify that resource. Each context key-value pair specifies a resource that contains the targeted resource.
--
-- /See:/ 'mkPhysicalResourceIdContextKeyValuePair' smart constructor.
data PhysicalResourceIdContextKeyValuePair = PhysicalResourceIdContextKeyValuePair'
  { key :: Types.Key
    -- ^ The resource context key.
  , value :: Types.Value
    -- ^ The resource context value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhysicalResourceIdContextKeyValuePair' value with any optional fields omitted.
mkPhysicalResourceIdContextKeyValuePair
    :: Types.Key -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> PhysicalResourceIdContextKeyValuePair
mkPhysicalResourceIdContextKeyValuePair key value
  = PhysicalResourceIdContextKeyValuePair'{key, value}

-- | The resource context key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prickvpKey :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Types.Key
prickvpKey = Lens.field @"key"
{-# INLINEABLE prickvpKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The resource context value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prickvpValue :: Lens.Lens' PhysicalResourceIdContextKeyValuePair Types.Value
prickvpValue = Lens.field @"value"
{-# INLINEABLE prickvpValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML PhysicalResourceIdContextKeyValuePair where
        parseXML x
          = PhysicalResourceIdContextKeyValuePair' Core.<$>
              (x Core..@ "Key") Core.<*> x Core..@ "Value"
