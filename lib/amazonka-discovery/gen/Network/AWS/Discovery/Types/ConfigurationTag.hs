{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationTag
  ( ConfigurationTag (..),

    -- * Smart constructor
    mkConfigurationTag,

    -- * Lenses
    ctConfigurationId,
    ctConfigurationType,
    ctKey,
    ctTimeOfCreation,
    ctValue,
  )
where

import qualified Network.AWS.Discovery.Types.ConfigurationId as Types
import qualified Network.AWS.Discovery.Types.ConfigurationItemType as Types
import qualified Network.AWS.Discovery.Types.Key as Types
import qualified Network.AWS.Discovery.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Tags for a configuration item. Tags are metadata that help you categorize IT assets.
--
-- /See:/ 'mkConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { -- | The configuration ID for the item to tag. You can specify a list of keys and values.
    configurationId :: Core.Maybe Types.ConfigurationId,
    -- | A type of IT asset to tag.
    configurationType :: Core.Maybe Types.ConfigurationItemType,
    -- | A type of tag on which to filter. For example, /serverType/ .
    key :: Core.Maybe Types.Key,
    -- | The time the configuration tag was created in Coordinated Universal Time (UTC).
    timeOfCreation :: Core.Maybe Core.NominalDiffTime,
    -- | A value on which to filter. For example /key = serverType/ and /value = web server/ .
    value :: Core.Maybe Types.TagValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ConfigurationTag' value with any optional fields omitted.
mkConfigurationTag ::
  ConfigurationTag
mkConfigurationTag =
  ConfigurationTag'
    { configurationId = Core.Nothing,
      configurationType = Core.Nothing,
      key = Core.Nothing,
      timeOfCreation = Core.Nothing,
      value = Core.Nothing
    }

-- | The configuration ID for the item to tag. You can specify a list of keys and values.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationId :: Lens.Lens' ConfigurationTag (Core.Maybe Types.ConfigurationId)
ctConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED ctConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | A type of IT asset to tag.
--
-- /Note:/ Consider using 'configurationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationType :: Lens.Lens' ConfigurationTag (Core.Maybe Types.ConfigurationItemType)
ctConfigurationType = Lens.field @"configurationType"
{-# DEPRECATED ctConfigurationType "Use generic-lens or generic-optics with 'configurationType' instead." #-}

-- | A type of tag on which to filter. For example, /serverType/ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKey :: Lens.Lens' ConfigurationTag (Core.Maybe Types.Key)
ctKey = Lens.field @"key"
{-# DEPRECATED ctKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The time the configuration tag was created in Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'timeOfCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTimeOfCreation :: Lens.Lens' ConfigurationTag (Core.Maybe Core.NominalDiffTime)
ctTimeOfCreation = Lens.field @"timeOfCreation"
{-# DEPRECATED ctTimeOfCreation "Use generic-lens or generic-optics with 'timeOfCreation' instead." #-}

-- | A value on which to filter. For example /key = serverType/ and /value = web server/ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctValue :: Lens.Lens' ConfigurationTag (Core.Maybe Types.TagValue)
ctValue = Lens.field @"value"
{-# DEPRECATED ctValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ConfigurationTag where
  parseJSON =
    Core.withObject "ConfigurationTag" Core.$
      \x ->
        ConfigurationTag'
          Core.<$> (x Core..:? "configurationId")
          Core.<*> (x Core..:? "configurationType")
          Core.<*> (x Core..:? "key")
          Core.<*> (x Core..:? "timeOfCreation")
          Core.<*> (x Core..:? "value")
