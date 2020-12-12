{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.GetLayerVersionResponse
  ( GetLayerVersionResponse (..),

    -- * Smart constructor
    mkGetLayerVersionResponse,

    -- * Lenses
    glvLayerVersionARN,
    glvContent,
    glvCreatedDate,
    glvVersion,
    glvLicenseInfo,
    glvLayerARN,
    glvDescription,
    glvCompatibleRuntimes,
  )
where

import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { layerVersionARN ::
      Lude.Maybe Lude.Text,
    content ::
      Lude.Maybe LayerVersionContentOutput,
    createdDate :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Integer,
    licenseInfo :: Lude.Maybe Lude.Text,
    layerARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    compatibleRuntimes :: Lude.Maybe [Runtime]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLayerVersionResponse' with the minimum fields required to make a request.
--
-- * 'compatibleRuntimes' - The layer's compatible runtimes.
-- * 'content' - Details about the layer version.
-- * 'createdDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
-- * 'description' - The description of the version.
-- * 'layerARN' - The ARN of the layer.
-- * 'layerVersionARN' - The ARN of the layer version.
-- * 'licenseInfo' - The layer's software license.
-- * 'version' - The version number.
mkGetLayerVersionResponse ::
  GetLayerVersionResponse
mkGetLayerVersionResponse =
  GetLayerVersionResponse'
    { layerVersionARN = Lude.Nothing,
      content = Lude.Nothing,
      createdDate = Lude.Nothing,
      version = Lude.Nothing,
      licenseInfo = Lude.Nothing,
      layerARN = Lude.Nothing,
      description = Lude.Nothing,
      compatibleRuntimes = Lude.Nothing
    }

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvLayerVersionARN :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Text)
glvLayerVersionARN = Lens.lens (layerVersionARN :: GetLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerVersionARN = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvLayerVersionARN "Use generic-lens or generic-optics with 'layerVersionARN' instead." #-}

-- | Details about the layer version.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvContent :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe LayerVersionContentOutput)
glvContent = Lens.lens (content :: GetLayerVersionResponse -> Lude.Maybe LayerVersionContentOutput) (\s a -> s {content = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvCreatedDate :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Text)
glvCreatedDate = Lens.lens (createdDate :: GetLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdDate = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvVersion :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Integer)
glvVersion = Lens.lens (version :: GetLayerVersionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The layer's software license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvLicenseInfo :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Text)
glvLicenseInfo = Lens.lens (licenseInfo :: GetLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseInfo = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

-- | The ARN of the layer.
--
-- /Note:/ Consider using 'layerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvLayerARN :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Text)
glvLayerARN = Lens.lens (layerARN :: GetLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerARN = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvLayerARN "Use generic-lens or generic-optics with 'layerARN' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvDescription :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe Lude.Text)
glvDescription = Lens.lens (description :: GetLayerVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvCompatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Lude.Maybe [Runtime])
glvCompatibleRuntimes = Lens.lens (compatibleRuntimes :: GetLayerVersionResponse -> Lude.Maybe [Runtime]) (\s a -> s {compatibleRuntimes = a} :: GetLayerVersionResponse)
{-# DEPRECATED glvCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

instance Lude.FromJSON GetLayerVersionResponse where
  parseJSON =
    Lude.withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            Lude.<$> (x Lude..:? "LayerVersionArn")
            Lude.<*> (x Lude..:? "Content")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "LicenseInfo")
            Lude.<*> (x Lude..:? "LayerArn")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CompatibleRuntimes" Lude..!= Lude.mempty)
      )
