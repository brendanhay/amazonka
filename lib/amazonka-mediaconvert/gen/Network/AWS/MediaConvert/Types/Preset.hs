{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Preset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Preset
  ( Preset (..)
  -- * Smart constructor
  , mkPreset
  -- * Lenses
  , pSettings
  , pName
  , pArn
  , pCategory
  , pCreatedAt
  , pDescription
  , pLastUpdated
  , pType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.PresetSettings as Types
import qualified Network.AWS.MediaConvert.Types.Type as Types
import qualified Network.AWS.Prelude as Core

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /See:/ 'mkPreset' smart constructor.
data Preset = Preset'
  { settings :: Types.PresetSettings
    -- ^ Settings for preset
  , name :: Core.Text
    -- ^ A name you create for each preset. Each name must be unique within your account.
  , arn :: Core.Maybe Core.Text
    -- ^ An identifier for this resource that is unique within all of AWS.
  , category :: Core.Maybe Core.Text
    -- ^ An optional category you create to organize your presets.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp in epoch seconds for preset creation.
  , description :: Core.Maybe Core.Text
    -- ^ An optional description you create for each preset.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp in epoch seconds when the preset was last updated.
  , type' :: Core.Maybe Types.Type
    -- ^ A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Preset' value with any optional fields omitted.
mkPreset
    :: Types.PresetSettings -- ^ 'settings'
    -> Core.Text -- ^ 'name'
    -> Preset
mkPreset settings name
  = Preset'{settings, name, arn = Core.Nothing,
            category = Core.Nothing, createdAt = Core.Nothing,
            description = Core.Nothing, lastUpdated = Core.Nothing,
            type' = Core.Nothing}

-- | Settings for preset
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSettings :: Lens.Lens' Preset Types.PresetSettings
pSettings = Lens.field @"settings"
{-# INLINEABLE pSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

-- | A name you create for each preset. Each name must be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Preset Core.Text
pName = Lens.field @"name"
{-# INLINEABLE pName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An identifier for this resource that is unique within all of AWS.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Preset (Core.Maybe Core.Text)
pArn = Lens.field @"arn"
{-# INLINEABLE pArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An optional category you create to organize your presets.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCategory :: Lens.Lens' Preset (Core.Maybe Core.Text)
pCategory = Lens.field @"category"
{-# INLINEABLE pCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The timestamp in epoch seconds for preset creation.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreatedAt :: Lens.Lens' Preset (Core.Maybe Core.NominalDiffTime)
pCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE pCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An optional description you create for each preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Preset (Core.Maybe Core.Text)
pDescription = Lens.field @"description"
{-# INLINEABLE pDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The timestamp in epoch seconds when the preset was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastUpdated :: Lens.Lens' Preset (Core.Maybe Core.NominalDiffTime)
pLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE pLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Preset (Core.Maybe Types.Type)
pType = Lens.field @"type'"
{-# INLINEABLE pType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Preset where
        parseJSON
          = Core.withObject "Preset" Core.$
              \ x ->
                Preset' Core.<$>
                  (x Core..: "settings") Core.<*> x Core..: "name" Core.<*>
                    x Core..:? "arn"
                    Core.<*> x Core..:? "category"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "lastUpdated"
                    Core.<*> x Core..:? "type"
