{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.LayerVersionContentOutput
  ( LayerVersionContentOutput (..)
  -- * Smart constructor
  , mkLayerVersionContentOutput
  -- * Lenses
  , lvcoCodeSha256
  , lvcoCodeSize
  , lvcoLocation
  , lvcoSigningJobArn
  , lvcoSigningProfileVersionArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { codeSha256 :: Core.Maybe Core.Text
    -- ^ The SHA-256 hash of the layer archive.
  , codeSize :: Core.Maybe Core.Integer
    -- ^ The size of the layer archive in bytes.
  , location :: Core.Maybe Core.Text
    -- ^ A link to the layer archive in Amazon S3 that is valid for 10 minutes.
  , signingJobArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of a signing job.
  , signingProfileVersionArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for a signing profile version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LayerVersionContentOutput' value with any optional fields omitted.
mkLayerVersionContentOutput
    :: LayerVersionContentOutput
mkLayerVersionContentOutput
  = LayerVersionContentOutput'{codeSha256 = Core.Nothing,
                               codeSize = Core.Nothing, location = Core.Nothing,
                               signingJobArn = Core.Nothing,
                               signingProfileVersionArn = Core.Nothing}

-- | The SHA-256 hash of the layer archive.
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoCodeSha256 :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
lvcoCodeSha256 = Lens.field @"codeSha256"
{-# INLINEABLE lvcoCodeSha256 #-}
{-# DEPRECATED codeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead"  #-}

-- | The size of the layer archive in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoCodeSize :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Integer)
lvcoCodeSize = Lens.field @"codeSize"
{-# INLINEABLE lvcoCodeSize #-}
{-# DEPRECATED codeSize "Use generic-lens or generic-optics with 'codeSize' instead"  #-}

-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoLocation :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
lvcoLocation = Lens.field @"location"
{-# INLINEABLE lvcoLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The Amazon Resource Name (ARN) of a signing job.
--
-- /Note:/ Consider using 'signingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoSigningJobArn :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
lvcoSigningJobArn = Lens.field @"signingJobArn"
{-# INLINEABLE lvcoSigningJobArn #-}
{-# DEPRECATED signingJobArn "Use generic-lens or generic-optics with 'signingJobArn' instead"  #-}

-- | The Amazon Resource Name (ARN) for a signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoSigningProfileVersionArn :: Lens.Lens' LayerVersionContentOutput (Core.Maybe Core.Text)
lvcoSigningProfileVersionArn = Lens.field @"signingProfileVersionArn"
{-# INLINEABLE lvcoSigningProfileVersionArn #-}
{-# DEPRECATED signingProfileVersionArn "Use generic-lens or generic-optics with 'signingProfileVersionArn' instead"  #-}

instance Core.FromJSON LayerVersionContentOutput where
        parseJSON
          = Core.withObject "LayerVersionContentOutput" Core.$
              \ x ->
                LayerVersionContentOutput' Core.<$>
                  (x Core..:? "CodeSha256") Core.<*> x Core..:? "CodeSize" Core.<*>
                    x Core..:? "Location"
                    Core.<*> x Core..:? "SigningJobArn"
                    Core.<*> x Core..:? "SigningProfileVersionArn"
