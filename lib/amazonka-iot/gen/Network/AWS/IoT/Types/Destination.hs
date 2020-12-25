{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dS3Destination,
  )
where

import qualified Network.AWS.IoT.Types.S3Destination as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the location of the updated firmware.
--
-- /See:/ 'mkDestination' smart constructor.
newtype Destination = Destination'
  { -- | Describes the location in S3 of the updated firmware.
    s3Destination :: Core.Maybe Types.S3Destination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Destination' value with any optional fields omitted.
mkDestination ::
  Destination
mkDestination = Destination' {s3Destination = Core.Nothing}

-- | Describes the location in S3 of the updated firmware.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dS3Destination :: Lens.Lens' Destination (Core.Maybe Types.S3Destination)
dS3Destination = Lens.field @"s3Destination"
{-# DEPRECATED dS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

instance Core.FromJSON Destination where
  toJSON Destination {..} =
    Core.object
      (Core.catMaybes [("s3Destination" Core..=) Core.<$> s3Destination])

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject "Destination" Core.$
      \x -> Destination' Core.<$> (x Core..:? "s3Destination")
