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
    papvePropertyValues,
    papveAssetId,
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
  )
where

import qualified Network.AWS.IoT.Types.AssetId as Types
import qualified Network.AWS.IoT.Types.AssetPropertyAlias as Types
import qualified Network.AWS.IoT.Types.AssetPropertyEntryId as Types
import qualified Network.AWS.IoT.Types.AssetPropertyValue as Types
import qualified Network.AWS.IoT.Types.PropertyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An asset property value entry containing the following information.
--
-- /See:/ 'mkPutAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { -- | A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
    propertyValues :: Core.NonEmpty Types.AssetPropertyValue,
    -- | The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
    assetId :: Core.Maybe Types.AssetId,
    -- | Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
    entryId :: Core.Maybe Types.AssetPropertyEntryId,
    -- | The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
    propertyAlias :: Core.Maybe Types.AssetPropertyAlias,
    -- | The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
    propertyId :: Core.Maybe Types.PropertyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAssetPropertyValueEntry' value with any optional fields omitted.
mkPutAssetPropertyValueEntry ::
  -- | 'propertyValues'
  Core.NonEmpty Types.AssetPropertyValue ->
  PutAssetPropertyValueEntry
mkPutAssetPropertyValueEntry propertyValues =
  PutAssetPropertyValueEntry'
    { propertyValues,
      assetId = Core.Nothing,
      entryId = Core.Nothing,
      propertyAlias = Core.Nothing,
      propertyId = Core.Nothing
    }

-- | A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
--
-- /Note:/ Consider using 'propertyValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyValues :: Lens.Lens' PutAssetPropertyValueEntry (Core.NonEmpty Types.AssetPropertyValue)
papvePropertyValues = Lens.field @"propertyValues"
{-# DEPRECATED papvePropertyValues "Use generic-lens or generic-optics with 'propertyValues' instead." #-}

-- | The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'assetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papveAssetId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Types.AssetId)
papveAssetId = Lens.field @"assetId"
{-# DEPRECATED papveAssetId "Use generic-lens or generic-optics with 'assetId' instead." #-}

-- | Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
--
-- /Note:/ Consider using 'entryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papveEntryId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Types.AssetPropertyEntryId)
papveEntryId = Lens.field @"entryId"
{-# DEPRECATED papveEntryId "Use generic-lens or generic-optics with 'entryId' instead." #-}

-- | The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'propertyAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyAlias :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Types.AssetPropertyAlias)
papvePropertyAlias = Lens.field @"propertyAlias"
{-# DEPRECATED papvePropertyAlias "Use generic-lens or generic-optics with 'propertyAlias' instead." #-}

-- | The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- /Note:/ Consider using 'propertyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papvePropertyId :: Lens.Lens' PutAssetPropertyValueEntry (Core.Maybe Types.PropertyId)
papvePropertyId = Lens.field @"propertyId"
{-# DEPRECATED papvePropertyId "Use generic-lens or generic-optics with 'propertyId' instead." #-}

instance Core.FromJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("propertyValues" Core..= propertyValues),
            ("assetId" Core..=) Core.<$> assetId,
            ("entryId" Core..=) Core.<$> entryId,
            ("propertyAlias" Core..=) Core.<$> propertyAlias,
            ("propertyId" Core..=) Core.<$> propertyId
          ]
      )

instance Core.FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    Core.withObject "PutAssetPropertyValueEntry" Core.$
      \x ->
        PutAssetPropertyValueEntry'
          Core.<$> (x Core..: "propertyValues")
          Core.<*> (x Core..:? "assetId")
          Core.<*> (x Core..:? "entryId")
          Core.<*> (x Core..:? "propertyAlias")
          Core.<*> (x Core..:? "propertyId")
