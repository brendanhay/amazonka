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
    lSigningProfileVersionARN,
    lARN,
    lSigningJobARN,
    lCodeSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayer' smart constructor.
data Layer = Layer'
  { -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the function layer.
    arn :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobARN :: Lude.Maybe Lude.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- * 'signingProfileVersionARN' - The Amazon Resource Name (ARN) for a signing profile version.
-- * 'arn' - The Amazon Resource Name (ARN) of the function layer.
-- * 'signingJobARN' - The Amazon Resource Name (ARN) of a signing job.
-- * 'codeSize' - The size of the layer archive in bytes.
mkLayer ::
  Layer
mkLayer =
  Layer'
    { signingProfileVersionARN = Lude.Nothing,
      arn = Lude.Nothing,
      signingJobARN = Lude.Nothing,
      codeSize = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSigningProfileVersionARN :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lSigningProfileVersionARN = Lens.lens (signingProfileVersionARN :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {signingProfileVersionARN = a} :: Layer)
{-# DEPRECATED lSigningProfileVersionARN "Use generic-lens or generic-optics with 'signingProfileVersionARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the function layer.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lARN :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lARN = Lens.lens (arn :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Layer)
{-# DEPRECATED lARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Amazon Resource Name (ARN) of a signing job.
--
-- /Note:/ Consider using 'signingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSigningJobARN :: Lens.Lens' Layer (Lude.Maybe Lude.Text)
lSigningJobARN = Lens.lens (signingJobARN :: Layer -> Lude.Maybe Lude.Text) (\s a -> s {signingJobARN = a} :: Layer)
{-# DEPRECATED lSigningJobARN "Use generic-lens or generic-optics with 'signingJobARN' instead." #-}

-- | The size of the layer archive in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCodeSize :: Lens.Lens' Layer (Lude.Maybe Lude.Integer)
lCodeSize = Lens.lens (codeSize :: Layer -> Lude.Maybe Lude.Integer) (\s a -> s {codeSize = a} :: Layer)
{-# DEPRECATED lCodeSize "Use generic-lens or generic-optics with 'codeSize' instead." #-}

instance Lude.FromJSON Layer where
  parseJSON =
    Lude.withObject
      "Layer"
      ( \x ->
          Layer'
            Lude.<$> (x Lude..:? "SigningProfileVersionArn")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "SigningJobArn")
            Lude.<*> (x Lude..:? "CodeSize")
      )
