{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DestinationSettings
  ( DestinationSettings (..)
  -- * Smart constructor
  , mkDestinationSettings
  -- * Lenses
  , dsS3Settings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.S3DestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /See:/ 'mkDestinationSettings' smart constructor.
newtype DestinationSettings = DestinationSettings'
  { s3Settings :: Core.Maybe Types.S3DestinationSettings
    -- ^ Settings associated with S3 destination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationSettings' value with any optional fields omitted.
mkDestinationSettings
    :: DestinationSettings
mkDestinationSettings
  = DestinationSettings'{s3Settings = Core.Nothing}

-- | Settings associated with S3 destination
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsS3Settings :: Lens.Lens' DestinationSettings (Core.Maybe Types.S3DestinationSettings)
dsS3Settings = Lens.field @"s3Settings"
{-# INLINEABLE dsS3Settings #-}
{-# DEPRECATED s3Settings "Use generic-lens or generic-optics with 's3Settings' instead"  #-}

instance Core.FromJSON DestinationSettings where
        toJSON DestinationSettings{..}
          = Core.object
              (Core.catMaybes [("s3Settings" Core..=) Core.<$> s3Settings])

instance Core.FromJSON DestinationSettings where
        parseJSON
          = Core.withObject "DestinationSettings" Core.$
              \ x -> DestinationSettings' Core.<$> (x Core..:? "s3Settings")
