{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.S3OutputUrl
  ( S3OutputUrl (..)
  -- * Smart constructor
  , mkS3OutputUrl
  -- * Lenses
  , souOutputUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Url as Types

-- | A URL for the S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkS3OutputUrl' smart constructor.
newtype S3OutputUrl = S3OutputUrl'
  { outputUrl :: Core.Maybe Types.Url
    -- ^ A URL for an S3 bucket where you want to store the results of this request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'S3OutputUrl' value with any optional fields omitted.
mkS3OutputUrl
    :: S3OutputUrl
mkS3OutputUrl = S3OutputUrl'{outputUrl = Core.Nothing}

-- | A URL for an S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
souOutputUrl :: Lens.Lens' S3OutputUrl (Core.Maybe Types.Url)
souOutputUrl = Lens.field @"outputUrl"
{-# INLINEABLE souOutputUrl #-}
{-# DEPRECATED outputUrl "Use generic-lens or generic-optics with 'outputUrl' instead"  #-}

instance Core.FromJSON S3OutputUrl where
        parseJSON
          = Core.withObject "S3OutputUrl" Core.$
              \ x -> S3OutputUrl' Core.<$> (x Core..:? "OutputUrl")
