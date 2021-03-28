{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.MetadataInfo
  ( MetadataInfo (..)
  -- * Smart constructor
  , mkMetadataInfo
  -- * Lenses
  , miCreatedTime
  , miMetadataValue
  ) where

import qualified Network.AWS.Glue.Types.CreatedTime as Types
import qualified Network.AWS.Glue.Types.MetadataValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure containing metadata information for a schema version.
--
-- /See:/ 'mkMetadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { createdTime :: Core.Maybe Types.CreatedTime
    -- ^ The time at which the entry was created.
  , metadataValue :: Core.Maybe Types.MetadataValueString
    -- ^ The metadata key’s corresponding value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetadataInfo' value with any optional fields omitted.
mkMetadataInfo
    :: MetadataInfo
mkMetadataInfo
  = MetadataInfo'{createdTime = Core.Nothing,
                  metadataValue = Core.Nothing}

-- | The time at which the entry was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miCreatedTime :: Lens.Lens' MetadataInfo (Core.Maybe Types.CreatedTime)
miCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE miCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The metadata key’s corresponding value.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miMetadataValue :: Lens.Lens' MetadataInfo (Core.Maybe Types.MetadataValueString)
miMetadataValue = Lens.field @"metadataValue"
{-# INLINEABLE miMetadataValue #-}
{-# DEPRECATED metadataValue "Use generic-lens or generic-optics with 'metadataValue' instead"  #-}

instance Core.FromJSON MetadataInfo where
        parseJSON
          = Core.withObject "MetadataInfo" Core.$
              \ x ->
                MetadataInfo' Core.<$>
                  (x Core..:? "CreatedTime") Core.<*> x Core..:? "MetadataValue"
