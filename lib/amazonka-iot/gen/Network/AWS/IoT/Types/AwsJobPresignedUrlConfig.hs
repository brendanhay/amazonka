{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
  ( AwsJobPresignedUrlConfig (..)
  -- * Smart constructor
  , mkAwsJobPresignedUrlConfig
  -- * Lenses
  , ajpucExpiresInSec
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for pre-signed URLs. Valid when @protocols@ contains HTTP.
--
-- /See:/ 'mkAwsJobPresignedUrlConfig' smart constructor.
newtype AwsJobPresignedUrlConfig = AwsJobPresignedUrlConfig'
  { expiresInSec :: Core.Maybe Core.Integer
    -- ^ How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobPresignedUrlConfig' value with any optional fields omitted.
mkAwsJobPresignedUrlConfig
    :: AwsJobPresignedUrlConfig
mkAwsJobPresignedUrlConfig
  = AwsJobPresignedUrlConfig'{expiresInSec = Core.Nothing}

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
--
-- /Note:/ Consider using 'expiresInSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajpucExpiresInSec :: Lens.Lens' AwsJobPresignedUrlConfig (Core.Maybe Core.Integer)
ajpucExpiresInSec = Lens.field @"expiresInSec"
{-# INLINEABLE ajpucExpiresInSec #-}
{-# DEPRECATED expiresInSec "Use generic-lens or generic-optics with 'expiresInSec' instead"  #-}

instance Core.FromJSON AwsJobPresignedUrlConfig where
        toJSON AwsJobPresignedUrlConfig{..}
          = Core.object
              (Core.catMaybes [("expiresInSec" Core..=) Core.<$> expiresInSec])

instance Core.FromJSON AwsJobPresignedUrlConfig where
        parseJSON
          = Core.withObject "AwsJobPresignedUrlConfig" Core.$
              \ x ->
                AwsJobPresignedUrlConfig' Core.<$> (x Core..:? "expiresInSec")
