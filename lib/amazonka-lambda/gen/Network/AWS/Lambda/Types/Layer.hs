{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Layer
  ( Layer (..),

    -- * Smart constructor
    mkLayer,

    -- * Lenses
    lArn,
    lCodeSize,
    lSigningJobArn,
    lSigningProfileVersionArn,
  )
where

import qualified Network.AWS.Lambda.Types.Arn as Types
import qualified Network.AWS.Lambda.Types.LayerVersionArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { -- | The Amazon Resource Name (ARN) of the function layer.
    arn :: Core.Maybe Types.LayerVersionArn,
    -- | The size of the layer archive in bytes.
    codeSize :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobArn :: Core.Maybe Types.Arn,
    -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Layer' value with any optional fields omitted.
mkLayer ::
  Layer
mkLayer =
  Layer'
    { arn = Core.Nothing,
      codeSize = Core.Nothing,
      signingJobArn = Core.Nothing,
      signingProfileVersionArn = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the function layer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lArn :: Lens.Lens' Layer (Core.Maybe Types.LayerVersionArn)
lArn = Lens.field @"arn"
{-# DEPRECATED lArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The size of the layer archive in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCodeSize :: Lens.Lens' Layer (Core.Maybe Core.Integer)
lCodeSize = Lens.field @"codeSize"
{-# DEPRECATED lCodeSize "Use generic-lens or generic-optics with 'codeSize' instead." #-}

-- | The Amazon Resource Name (ARN) of a signing job.
--
-- /Note:/ Consider using 'signingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSigningJobArn :: Lens.Lens' Layer (Core.Maybe Types.Arn)
lSigningJobArn = Lens.field @"signingJobArn"
{-# DEPRECATED lSigningJobArn "Use generic-lens or generic-optics with 'signingJobArn' instead." #-}

-- | The Amazon Resource Name (ARN) for a signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSigningProfileVersionArn :: Lens.Lens' Layer (Core.Maybe Types.Arn)
lSigningProfileVersionArn = Lens.field @"signingProfileVersionArn"
{-# DEPRECATED lSigningProfileVersionArn "Use generic-lens or generic-optics with 'signingProfileVersionArn' instead." #-}

instance Core.FromJSON Layer where
  parseJSON =
    Core.withObject "Layer" Core.$
      \x ->
        Layer'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "CodeSize")
          Core.<*> (x Core..:? "SigningJobArn")
          Core.<*> (x Core..:? "SigningProfileVersionArn")
