{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TimeToLiveSpecification
  ( TimeToLiveSpecification (..)
  -- * Smart constructor
  , mkTimeToLiveSpecification
  -- * Lenses
  , ttlsEnabled
  , ttlsAttributeName
  ) where

import qualified Network.AWS.DynamoDB.Types.TimeToLiveAttributeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings used to enable or disable Time to Live (TTL) for the specified table.
--
-- /See:/ 'mkTimeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { enabled :: Core.Bool
    -- ^ Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
  , attributeName :: Types.TimeToLiveAttributeName
    -- ^ The name of the TTL attribute used to store the expiration time for items in the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimeToLiveSpecification' value with any optional fields omitted.
mkTimeToLiveSpecification
    :: Core.Bool -- ^ 'enabled'
    -> Types.TimeToLiveAttributeName -- ^ 'attributeName'
    -> TimeToLiveSpecification
mkTimeToLiveSpecification enabled attributeName
  = TimeToLiveSpecification'{enabled, attributeName}

-- | Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttlsEnabled :: Lens.Lens' TimeToLiveSpecification Core.Bool
ttlsEnabled = Lens.field @"enabled"
{-# INLINEABLE ttlsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The name of the TTL attribute used to store the expiration time for items in the table.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttlsAttributeName :: Lens.Lens' TimeToLiveSpecification Types.TimeToLiveAttributeName
ttlsAttributeName = Lens.field @"attributeName"
{-# INLINEABLE ttlsAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

instance Core.FromJSON TimeToLiveSpecification where
        toJSON TimeToLiveSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Enabled" Core..= enabled),
                  Core.Just ("AttributeName" Core..= attributeName)])

instance Core.FromJSON TimeToLiveSpecification where
        parseJSON
          = Core.withObject "TimeToLiveSpecification" Core.$
              \ x ->
                TimeToLiveSpecification' Core.<$>
                  (x Core..: "Enabled") Core.<*> x Core..: "AttributeName"
