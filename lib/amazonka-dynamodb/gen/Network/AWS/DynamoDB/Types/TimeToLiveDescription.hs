{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TimeToLiveDescription
  ( TimeToLiveDescription (..)
  -- * Smart constructor
  , mkTimeToLiveDescription
  -- * Lenses
  , ttldAttributeName
  , ttldTimeToLiveStatus
  ) where

import qualified Network.AWS.DynamoDB.Types.TimeToLiveAttributeName as Types
import qualified Network.AWS.DynamoDB.Types.TimeToLiveStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of the Time to Live (TTL) status on the specified table. 
--
-- /See:/ 'mkTimeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { attributeName :: Core.Maybe Types.TimeToLiveAttributeName
    -- ^ The name of the TTL attribute for items in the table.
  , timeToLiveStatus :: Core.Maybe Types.TimeToLiveStatus
    -- ^ The TTL status for the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimeToLiveDescription' value with any optional fields omitted.
mkTimeToLiveDescription
    :: TimeToLiveDescription
mkTimeToLiveDescription
  = TimeToLiveDescription'{attributeName = Core.Nothing,
                           timeToLiveStatus = Core.Nothing}

-- | The name of the TTL attribute for items in the table.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttldAttributeName :: Lens.Lens' TimeToLiveDescription (Core.Maybe Types.TimeToLiveAttributeName)
ttldAttributeName = Lens.field @"attributeName"
{-# INLINEABLE ttldAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The TTL status for the table.
--
-- /Note:/ Consider using 'timeToLiveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttldTimeToLiveStatus :: Lens.Lens' TimeToLiveDescription (Core.Maybe Types.TimeToLiveStatus)
ttldTimeToLiveStatus = Lens.field @"timeToLiveStatus"
{-# INLINEABLE ttldTimeToLiveStatus #-}
{-# DEPRECATED timeToLiveStatus "Use generic-lens or generic-optics with 'timeToLiveStatus' instead"  #-}

instance Core.FromJSON TimeToLiveDescription where
        parseJSON
          = Core.withObject "TimeToLiveDescription" Core.$
              \ x ->
                TimeToLiveDescription' Core.<$>
                  (x Core..:? "AttributeName") Core.<*> x Core..:? "TimeToLiveStatus"
