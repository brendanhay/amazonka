{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AwsIamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.AwsIamConfig
  ( AwsIamConfig (..)
  -- * Smart constructor
  , mkAwsIamConfig
  -- * Lenses
  , aicSigningRegion
  , aicSigningServiceName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS IAM configuration.
--
-- /See:/ 'mkAwsIamConfig' smart constructor.
data AwsIamConfig = AwsIamConfig'
  { signingRegion :: Core.Maybe Core.Text
    -- ^ The signing region for AWS IAM authorization.
  , signingServiceName :: Core.Maybe Core.Text
    -- ^ The signing service name for AWS IAM authorization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsIamConfig' value with any optional fields omitted.
mkAwsIamConfig
    :: AwsIamConfig
mkAwsIamConfig
  = AwsIamConfig'{signingRegion = Core.Nothing,
                  signingServiceName = Core.Nothing}

-- | The signing region for AWS IAM authorization.
--
-- /Note:/ Consider using 'signingRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicSigningRegion :: Lens.Lens' AwsIamConfig (Core.Maybe Core.Text)
aicSigningRegion = Lens.field @"signingRegion"
{-# INLINEABLE aicSigningRegion #-}
{-# DEPRECATED signingRegion "Use generic-lens or generic-optics with 'signingRegion' instead"  #-}

-- | The signing service name for AWS IAM authorization.
--
-- /Note:/ Consider using 'signingServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicSigningServiceName :: Lens.Lens' AwsIamConfig (Core.Maybe Core.Text)
aicSigningServiceName = Lens.field @"signingServiceName"
{-# INLINEABLE aicSigningServiceName #-}
{-# DEPRECATED signingServiceName "Use generic-lens or generic-optics with 'signingServiceName' instead"  #-}

instance Core.FromJSON AwsIamConfig where
        toJSON AwsIamConfig{..}
          = Core.object
              (Core.catMaybes
                 [("signingRegion" Core..=) Core.<$> signingRegion,
                  ("signingServiceName" Core..=) Core.<$> signingServiceName])

instance Core.FromJSON AwsIamConfig where
        parseJSON
          = Core.withObject "AwsIamConfig" Core.$
              \ x ->
                AwsIamConfig' Core.<$>
                  (x Core..:? "signingRegion") Core.<*>
                    x Core..:? "signingServiceName"
