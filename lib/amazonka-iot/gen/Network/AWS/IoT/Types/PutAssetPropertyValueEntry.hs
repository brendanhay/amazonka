{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PutAssetPropertyValueEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutAssetPropertyValueEntry
  ( PutAssetPropertyValueEntry (..),

    -- * Smart constructor
    mkPutAssetPropertyValueEntry,

    -- * Lenses
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
    papveAssetId,
    papvePropertyValues,
  )
where

import Network.AWS.IoT.Types.AssetPropertyValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An asset property value entry containing the following information.
--
-- /See:/ 'mkPutAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { entryId ::
      Lude.Maybe Lude.Text,
    propertyAlias :: Lude.Maybe Lude.Text,
    propertyId :: Lude.Maybe Lude.Text,
    assetId :: Lude.Maybe Lude.Text,
    propertyValues ::
      Lude.NonEmpty AssetPropertyValue
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAssetPropertyValueEntry' with the minimum fields required to make a request.
--
-- * 'assetId' - The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
-- * 'entryId' - Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
-- * 'propertyAlias' - The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
-- * 'propertyId' - The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
-- * 'propertyValues' - A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
mkPutAssetPropertyValueEntry ::
  -- | 'propertyValues'
  Lude.NonEmpty AssetPropertyValue ->
  PutAssetPropertyValueEntry
mkPutAssetPropertyValueEntry pPropertyValues_ =
  PutAssetPropertyValueEntry'
    { entryId = Lude.Nothing,
      propertyAlias = Lude.Nothing,
      propertyId = Lude.Nothing,
      assetId = Lude.Nothing,
      propertyValues = pPropertyValues_
    }

-- | Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
--
-- /Note:/ Consider using 'entryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papveEntryId :: Lens.Lens' PutAssetPropertyValueEntry (Lude.Maybe Lude.Text)
papveEntryId = Lens.lens (entryId :: PutAssetPropertyValueEntry -> Lude.Maybe Lude.Text) (\s a -> s {entryId = a} :: PutAssetPropertyValueEntry)
{-# DEPRECATED papveEntryId "Use generic-lens or generic-optics with 'entryId' instead." #-}

-- | The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'propertyAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyAlias :: Lens.Lens' PutAssetPropertyValueEntry (Lude.Maybe Lude.Text)
papvePropertyAlias = Lens.lens (propertyAlias :: PutAssetPropertyValueEntry -> Lude.Maybe Lude.Text) (\s a -> s {propertyAlias = a} :: PutAssetPropertyValueEntry)
{-# DEPRECATED papvePropertyAlias "Use generic-lens or generic-optics with 'propertyAlias' instead." #-}

-- | The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'propertyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyId :: Lens.Lens' PutAssetPropertyValueEntry (Lude.Maybe Lude.Text)
papvePropertyId = Lens.lens (propertyId :: PutAssetPropertyValueEntry -> Lude.Maybe Lude.Text) (\s a -> s {propertyId = a} :: PutAssetPropertyValueEntry)
{-# DEPRECATED papvePropertyId "Use generic-lens or generic-optics with 'propertyId' instead." #-}

-- | The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'assetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papveAssetId :: Lens.Lens' PutAssetPropertyValueEntry (Lude.Maybe Lude.Text)
papveAssetId = Lens.lens (assetId :: PutAssetPropertyValueEntry -> Lude.Maybe Lude.Text) (\s a -> s {assetId = a} :: PutAssetPropertyValueEntry)
{-# DEPRECATED papveAssetId "Use generic-lens or generic-optics with 'assetId' instead." #-}

-- | A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
--
-- /Note:/ Consider using 'propertyValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyValues :: Lens.Lens' PutAssetPropertyValueEntry (Lude.NonEmpty AssetPropertyValue)
papvePropertyValues = Lens.lens (propertyValues :: PutAssetPropertyValueEntry -> Lude.NonEmpty AssetPropertyValue) (\s a -> s {propertyValues = a} :: PutAssetPropertyValueEntry)
{-# DEPRECATED papvePropertyValues "Use generic-lens or generic-optics with 'propertyValues' instead." #-}

instance Lude.FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    Lude.withObject
      "PutAssetPropertyValueEntry"
      ( \x ->
          PutAssetPropertyValueEntry'
            Lude.<$> (x Lude..:? "entryId")
            Lude.<*> (x Lude..:? "propertyAlias")
            Lude.<*> (x Lude..:? "propertyId")
            Lude.<*> (x Lude..:? "assetId")
            Lude.<*> (x Lude..: "propertyValues")
      )

instance Lude.ToJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("entryId" Lude..=) Lude.<$> entryId,
            ("propertyAlias" Lude..=) Lude.<$> propertyAlias,
            ("propertyId" Lude..=) Lude.<$> propertyId,
            ("assetId" Lude..=) Lude.<$> assetId,
            Lude.Just ("propertyValues" Lude..= propertyValues)
          ]
      )
