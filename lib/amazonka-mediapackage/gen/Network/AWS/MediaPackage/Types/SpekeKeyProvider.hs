{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.SpekeKeyProvider
  ( SpekeKeyProvider (..)
  -- * Smart constructor
  , mkSpekeKeyProvider
  -- * Lenses
  , skpResourceId
  , skpSystemIds
  , skpUrl
  , skpRoleArn
  , skpCertificateArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'mkSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { resourceId :: Core.Text
    -- ^ The resource ID to include in key requests.
  , systemIds :: [Core.Text]
    -- ^ The system IDs to include in key requests.
  , url :: Core.Text
    -- ^ The URL of the external key provider service.
  , roleArn :: Core.Text
    -- ^ An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
--
-- MediaPackage will assume when accessing the key provider service.
  , certificateArn :: Core.Maybe Core.Text
    -- ^ An Amazon Resource Name (ARN) of a Certificate Manager certificate
--
-- that MediaPackage will use for enforcing secure end-to-end data
-- transfer with the key provider service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpekeKeyProvider' value with any optional fields omitted.
mkSpekeKeyProvider
    :: Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'url'
    -> Core.Text -- ^ 'roleArn'
    -> SpekeKeyProvider
mkSpekeKeyProvider resourceId url roleArn
  = SpekeKeyProvider'{resourceId, systemIds = Core.mempty, url,
                      roleArn, certificateArn = Core.Nothing}

-- | The resource ID to include in key requests.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpResourceId :: Lens.Lens' SpekeKeyProvider Core.Text
skpResourceId = Lens.field @"resourceId"
{-# INLINEABLE skpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The system IDs to include in key requests.
--
-- /Note:/ Consider using 'systemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpSystemIds :: Lens.Lens' SpekeKeyProvider [Core.Text]
skpSystemIds = Lens.field @"systemIds"
{-# INLINEABLE skpSystemIds #-}
{-# DEPRECATED systemIds "Use generic-lens or generic-optics with 'systemIds' instead"  #-}

-- | The URL of the external key provider service.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpUrl :: Lens.Lens' SpekeKeyProvider Core.Text
skpUrl = Lens.field @"url"
{-# INLINEABLE skpUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental
--
-- MediaPackage will assume when accessing the key provider service.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpRoleArn :: Lens.Lens' SpekeKeyProvider Core.Text
skpRoleArn = Lens.field @"roleArn"
{-# INLINEABLE skpRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | An Amazon Resource Name (ARN) of a Certificate Manager certificate
--
-- that MediaPackage will use for enforcing secure end-to-end data
-- transfer with the key provider service.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpCertificateArn :: Lens.Lens' SpekeKeyProvider (Core.Maybe Core.Text)
skpCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE skpCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.FromJSON SpekeKeyProvider where
        toJSON SpekeKeyProvider{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("resourceId" Core..= resourceId),
                  Core.Just ("systemIds" Core..= systemIds),
                  Core.Just ("url" Core..= url),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("certificateArn" Core..=) Core.<$> certificateArn])

instance Core.FromJSON SpekeKeyProvider where
        parseJSON
          = Core.withObject "SpekeKeyProvider" Core.$
              \ x ->
                SpekeKeyProvider' Core.<$>
                  (x Core..: "resourceId") Core.<*>
                    x Core..:? "systemIds" Core..!= Core.mempty
                    Core.<*> x Core..: "url"
                    Core.<*> x Core..: "roleArn"
                    Core.<*> x Core..:? "certificateArn"
