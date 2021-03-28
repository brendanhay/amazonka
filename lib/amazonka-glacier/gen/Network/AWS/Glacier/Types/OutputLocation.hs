{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.OutputLocation
  ( OutputLocation (..)
  -- * Smart constructor
  , mkOutputLocation
  -- * Lenses
  , olS3
  ) where

import qualified Network.AWS.Glacier.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the location where the select job results are stored.
--
-- /See:/ 'mkOutputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { s3 :: Core.Maybe Types.S3Location
    -- ^ Describes an S3 location that will receive the results of the job request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputLocation' value with any optional fields omitted.
mkOutputLocation
    :: OutputLocation
mkOutputLocation = OutputLocation'{s3 = Core.Nothing}

-- | Describes an S3 location that will receive the results of the job request.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olS3 :: Lens.Lens' OutputLocation (Core.Maybe Types.S3Location)
olS3 = Lens.field @"s3"
{-# INLINEABLE olS3 #-}
{-# DEPRECATED s3 "Use generic-lens or generic-optics with 's3' instead"  #-}

instance Core.FromJSON OutputLocation where
        toJSON OutputLocation{..}
          = Core.object (Core.catMaybes [("S3" Core..=) Core.<$> s3])

instance Core.FromJSON OutputLocation where
        parseJSON
          = Core.withObject "OutputLocation" Core.$
              \ x -> OutputLocation' Core.<$> (x Core..:? "S3")
