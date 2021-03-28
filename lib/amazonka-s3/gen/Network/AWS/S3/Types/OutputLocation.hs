{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.OutputLocation
  ( OutputLocation (..)
  -- * Smart constructor
  , mkOutputLocation
  -- * Lenses
  , olS3
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.S3Location as Types

-- | Describes the location where the restore job's output is stored.
--
-- /See:/ 'mkOutputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { s3 :: Core.Maybe Types.S3Location
    -- ^ Describes an S3 location that will receive the results of the restore request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputLocation' value with any optional fields omitted.
mkOutputLocation
    :: OutputLocation
mkOutputLocation = OutputLocation'{s3 = Core.Nothing}

-- | Describes an S3 location that will receive the results of the restore request.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olS3 :: Lens.Lens' OutputLocation (Core.Maybe Types.S3Location)
olS3 = Lens.field @"s3"
{-# INLINEABLE olS3 #-}
{-# DEPRECATED s3 "Use generic-lens or generic-optics with 's3' instead"  #-}

instance Core.ToXML OutputLocation where
        toXML OutputLocation{..}
          = Core.maybe Core.mempty (Core.toXMLElement "S3") s3
