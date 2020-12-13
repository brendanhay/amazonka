{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentOutput
  ( LayerVersionContentOutput (..),

    -- * Smart constructor
    mkLayerVersionContentOutput,

    -- * Lenses
    lvcoSigningProfileVersionARN,
    lvcoLocation,
    lvcoSigningJobARN,
    lvcoCodeSize,
    lvcoCodeSha256,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionARN :: Lude.Maybe Lude.Text,
    -- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
    location :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobARN :: Lude.Maybe Lude.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Lude.Maybe Lude.Integer,
    -- | The SHA-256 hash of the layer archive.
    codeSha256 :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LayerVersionContentOutput' with the minimum fields required to make a request.
--
-- * 'signingProfileVersionARN' - The Amazon Resource Name (ARN) for a signing profile version.
-- * 'location' - A link to the layer archive in Amazon S3 that is valid for 10 minutes.
-- * 'signingJobARN' - The Amazon Resource Name (ARN) of a signing job.
-- * 'codeSize' - The size of the layer archive in bytes.
-- * 'codeSha256' - The SHA-256 hash of the layer archive.
mkLayerVersionContentOutput ::
  LayerVersionContentOutput
mkLayerVersionContentOutput =
  LayerVersionContentOutput'
    { signingProfileVersionARN =
        Lude.Nothing,
      location = Lude.Nothing,
      signingJobARN = Lude.Nothing,
      codeSize = Lude.Nothing,
      codeSha256 = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
--
-- /Note:/ Consider using 'signingProfileVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoSigningProfileVersionARN :: Lens.Lens' LayerVersionContentOutput (Lude.Maybe Lude.Text)
lvcoSigningProfileVersionARN = Lens.lens (signingProfileVersionARN :: LayerVersionContentOutput -> Lude.Maybe Lude.Text) (\s a -> s {signingProfileVersionARN = a} :: LayerVersionContentOutput)
{-# DEPRECATED lvcoSigningProfileVersionARN "Use generic-lens or generic-optics with 'signingProfileVersionARN' instead." #-}

-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoLocation :: Lens.Lens' LayerVersionContentOutput (Lude.Maybe Lude.Text)
lvcoLocation = Lens.lens (location :: LayerVersionContentOutput -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: LayerVersionContentOutput)
{-# DEPRECATED lvcoLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The Amazon Resource Name (ARN) of a signing job.
--
-- /Note:/ Consider using 'signingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoSigningJobARN :: Lens.Lens' LayerVersionContentOutput (Lude.Maybe Lude.Text)
lvcoSigningJobARN = Lens.lens (signingJobARN :: LayerVersionContentOutput -> Lude.Maybe Lude.Text) (\s a -> s {signingJobARN = a} :: LayerVersionContentOutput)
{-# DEPRECATED lvcoSigningJobARN "Use generic-lens or generic-optics with 'signingJobARN' instead." #-}

-- | The size of the layer archive in bytes.
--
-- /Note:/ Consider using 'codeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoCodeSize :: Lens.Lens' LayerVersionContentOutput (Lude.Maybe Lude.Integer)
lvcoCodeSize = Lens.lens (codeSize :: LayerVersionContentOutput -> Lude.Maybe Lude.Integer) (\s a -> s {codeSize = a} :: LayerVersionContentOutput)
{-# DEPRECATED lvcoCodeSize "Use generic-lens or generic-optics with 'codeSize' instead." #-}

-- | The SHA-256 hash of the layer archive.
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvcoCodeSha256 :: Lens.Lens' LayerVersionContentOutput (Lude.Maybe Lude.Text)
lvcoCodeSha256 = Lens.lens (codeSha256 :: LayerVersionContentOutput -> Lude.Maybe Lude.Text) (\s a -> s {codeSha256 = a} :: LayerVersionContentOutput)
{-# DEPRECATED lvcoCodeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead." #-}

instance Lude.FromJSON LayerVersionContentOutput where
  parseJSON =
    Lude.withObject
      "LayerVersionContentOutput"
      ( \x ->
          LayerVersionContentOutput'
            Lude.<$> (x Lude..:? "SigningProfileVersionArn")
            Lude.<*> (x Lude..:? "Location")
            Lude.<*> (x Lude..:? "SigningJobArn")
            Lude.<*> (x Lude..:? "CodeSize")
            Lude.<*> (x Lude..:? "CodeSha256")
      )
