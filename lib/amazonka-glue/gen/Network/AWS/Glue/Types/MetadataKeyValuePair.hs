{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.MetadataKeyValuePair
  ( MetadataKeyValuePair (..)
  -- * Smart constructor
  , mkMetadataKeyValuePair
  -- * Lenses
  , mkvpMetadataKey
  , mkvpMetadataValue
  ) where

import qualified Network.AWS.Glue.Types.MetadataKey as Types
import qualified Network.AWS.Glue.Types.MetadataValueString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure containing a key value pair for metadata.
--
-- /See:/ 'mkMetadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { metadataKey :: Core.Maybe Types.MetadataKey
    -- ^ A metadata key.
  , metadataValue :: Core.Maybe Types.MetadataValueString
    -- ^ A metadata key’s corresponding value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetadataKeyValuePair' value with any optional fields omitted.
mkMetadataKeyValuePair
    :: MetadataKeyValuePair
mkMetadataKeyValuePair
  = MetadataKeyValuePair'{metadataKey = Core.Nothing,
                          metadataValue = Core.Nothing}

-- | A metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mkvpMetadataKey :: Lens.Lens' MetadataKeyValuePair (Core.Maybe Types.MetadataKey)
mkvpMetadataKey = Lens.field @"metadataKey"
{-# INLINEABLE mkvpMetadataKey #-}
{-# DEPRECATED metadataKey "Use generic-lens or generic-optics with 'metadataKey' instead"  #-}

-- | A metadata key’s corresponding value.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mkvpMetadataValue :: Lens.Lens' MetadataKeyValuePair (Core.Maybe Types.MetadataValueString)
mkvpMetadataValue = Lens.field @"metadataValue"
{-# INLINEABLE mkvpMetadataValue #-}
{-# DEPRECATED metadataValue "Use generic-lens or generic-optics with 'metadataValue' instead"  #-}

instance Core.FromJSON MetadataKeyValuePair where
        toJSON MetadataKeyValuePair{..}
          = Core.object
              (Core.catMaybes
                 [("MetadataKey" Core..=) Core.<$> metadataKey,
                  ("MetadataValue" Core..=) Core.<$> metadataValue])
