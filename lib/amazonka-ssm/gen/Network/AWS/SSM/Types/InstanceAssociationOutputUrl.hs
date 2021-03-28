{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstanceAssociationOutputUrl
  ( InstanceAssociationOutputUrl (..)
  -- * Smart constructor
  , mkInstanceAssociationOutputUrl
  -- * Lenses
  , iaouS3OutputUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.S3OutputUrl as Types

-- | The URL of S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkInstanceAssociationOutputUrl' smart constructor.
newtype InstanceAssociationOutputUrl = InstanceAssociationOutputUrl'
  { s3OutputUrl :: Core.Maybe Types.S3OutputUrl
    -- ^ The URL of S3 bucket where you want to store the results of this request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceAssociationOutputUrl' value with any optional fields omitted.
mkInstanceAssociationOutputUrl
    :: InstanceAssociationOutputUrl
mkInstanceAssociationOutputUrl
  = InstanceAssociationOutputUrl'{s3OutputUrl = Core.Nothing}

-- | The URL of S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 's3OutputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaouS3OutputUrl :: Lens.Lens' InstanceAssociationOutputUrl (Core.Maybe Types.S3OutputUrl)
iaouS3OutputUrl = Lens.field @"s3OutputUrl"
{-# INLINEABLE iaouS3OutputUrl #-}
{-# DEPRECATED s3OutputUrl "Use generic-lens or generic-optics with 's3OutputUrl' instead"  #-}

instance Core.FromJSON InstanceAssociationOutputUrl where
        parseJSON
          = Core.withObject "InstanceAssociationOutputUrl" Core.$
              \ x ->
                InstanceAssociationOutputUrl' Core.<$> (x Core..:? "S3OutputUrl")
