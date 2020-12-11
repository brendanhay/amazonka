-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionsListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionsListItem
  ( LayerVersionsListItem (..),

    -- * Smart constructor
    mkLayerVersionsListItem,

    -- * Lenses
    lvliLayerVersionARN,
    lvliCreatedDate,
    lvliVersion,
    lvliLicenseInfo,
    lvliDescription,
    lvliCompatibleRuntimes,
  )
where

import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { layerVersionARN ::
      Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Integer,
    licenseInfo :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LayerVersionsListItem' with the minimum fields required to make a request.
--
-- * 'compatibleRuntimes' - The layer's compatible runtimes.
-- * 'createdDate' - The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
-- * 'description' - The description of the version.
-- * 'layerVersionARN' - The ARN of the layer version.
-- * 'licenseInfo' - The layer's open-source license.
-- * 'version' - The version number.
mkLayerVersionsListItem ::
  LayerVersionsListItem
mkLayerVersionsListItem =
  LayerVersionsListItem'
    { layerVersionARN = Lude.Nothing,
      createdDate = Lude.Nothing,
      version = Lude.Nothing,
      licenseInfo = Lude.Nothing,
      description = Lude.Nothing,
      compatibleRuntimes = Lude.Nothing
    }

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliLayerVersionARN :: Lens.Lens' LayerVersionsListItem (Lude.Maybe Lude.Text)
lvliLayerVersionARN = Lens.lens (layerVersionARN :: LayerVersionsListItem -> Lude.Maybe Lude.Text) (\s a -> s {layerVersionARN = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliLayerVersionARN "Use generic-lens or generic-optics with 'layerVersionARN' instead." #-}

-- | The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliCreatedDate :: Lens.Lens' LayerVersionsListItem (Lude.Maybe Lude.Text)
lvliCreatedDate = Lens.lens (createdDate :: LayerVersionsListItem -> Lude.Maybe Lude.Text) (\s a -> s {createdDate = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliVersion :: Lens.Lens' LayerVersionsListItem (Lude.Maybe Lude.Integer)
lvliVersion = Lens.lens (version :: LayerVersionsListItem -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The layer's open-source license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliLicenseInfo :: Lens.Lens' LayerVersionsListItem (Lude.Maybe Lude.Text)
lvliLicenseInfo = Lens.lens (licenseInfo :: LayerVersionsListItem -> Lude.Maybe Lude.Text) (\s a -> s {licenseInfo = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliLicenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead." #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliDescription :: Lens.Lens' LayerVersionsListItem (Lude.Maybe Lude.Text)
lvliDescription = Lens.lens (description :: LayerVersionsListItem -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliCompatibleRuntimes :: Lens.Lens' LayerVersionsListItem (Lude.Maybe [Runtime])
lvliCompatibleRuntimes = Lens.lens (compatibleRuntimes :: LayerVersionsListItem -> Lude.Maybe [Runtime]) (\s a -> s {compatibleRuntimes = a} :: LayerVersionsListItem)
{-# DEPRECATED lvliCompatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead." #-}

instance Lude.FromJSON LayerVersionsListItem where
  parseJSON =
    Lude.withObject
      "LayerVersionsListItem"
      ( \x ->
          LayerVersionsListItem'
            Lude.<$> (x Lude..:? "LayerVersionArn")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "LicenseInfo")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CompatibleRuntimes" Lude..!= Lude.mempty)
      )
