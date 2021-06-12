{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationTag where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types.ConfigurationItemType
import qualified Network.AWS.Lens as Lens

-- | Tags for a configuration item. Tags are metadata that help you
-- categorize IT assets.
--
-- /See:/ 'newConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { -- | A type of tag on which to filter. For example, /serverType/.
    key :: Core.Maybe Core.Text,
    -- | The configuration ID for the item to tag. You can specify a list of keys
    -- and values.
    configurationId :: Core.Maybe Core.Text,
    -- | A value on which to filter. For example /key = serverType/ and /value =
    -- web server/.
    value :: Core.Maybe Core.Text,
    -- | A type of IT asset to tag.
    configurationType :: Core.Maybe ConfigurationItemType,
    -- | The time the configuration tag was created in Coordinated Universal Time
    -- (UTC).
    timeOfCreation :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigurationTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'configurationTag_key' - A type of tag on which to filter. For example, /serverType/.
--
-- 'configurationId', 'configurationTag_configurationId' - The configuration ID for the item to tag. You can specify a list of keys
-- and values.
--
-- 'value', 'configurationTag_value' - A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
--
-- 'configurationType', 'configurationTag_configurationType' - A type of IT asset to tag.
--
-- 'timeOfCreation', 'configurationTag_timeOfCreation' - The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
newConfigurationTag ::
  ConfigurationTag
newConfigurationTag =
  ConfigurationTag'
    { key = Core.Nothing,
      configurationId = Core.Nothing,
      value = Core.Nothing,
      configurationType = Core.Nothing,
      timeOfCreation = Core.Nothing
    }

-- | A type of tag on which to filter. For example, /serverType/.
configurationTag_key :: Lens.Lens' ConfigurationTag (Core.Maybe Core.Text)
configurationTag_key = Lens.lens (\ConfigurationTag' {key} -> key) (\s@ConfigurationTag' {} a -> s {key = a} :: ConfigurationTag)

-- | The configuration ID for the item to tag. You can specify a list of keys
-- and values.
configurationTag_configurationId :: Lens.Lens' ConfigurationTag (Core.Maybe Core.Text)
configurationTag_configurationId = Lens.lens (\ConfigurationTag' {configurationId} -> configurationId) (\s@ConfigurationTag' {} a -> s {configurationId = a} :: ConfigurationTag)

-- | A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
configurationTag_value :: Lens.Lens' ConfigurationTag (Core.Maybe Core.Text)
configurationTag_value = Lens.lens (\ConfigurationTag' {value} -> value) (\s@ConfigurationTag' {} a -> s {value = a} :: ConfigurationTag)

-- | A type of IT asset to tag.
configurationTag_configurationType :: Lens.Lens' ConfigurationTag (Core.Maybe ConfigurationItemType)
configurationTag_configurationType = Lens.lens (\ConfigurationTag' {configurationType} -> configurationType) (\s@ConfigurationTag' {} a -> s {configurationType = a} :: ConfigurationTag)

-- | The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
configurationTag_timeOfCreation :: Lens.Lens' ConfigurationTag (Core.Maybe Core.UTCTime)
configurationTag_timeOfCreation = Lens.lens (\ConfigurationTag' {timeOfCreation} -> timeOfCreation) (\s@ConfigurationTag' {} a -> s {timeOfCreation = a} :: ConfigurationTag) Core.. Lens.mapping Core._Time

instance Core.FromJSON ConfigurationTag where
  parseJSON =
    Core.withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            Core.<$> (x Core..:? "key")
            Core.<*> (x Core..:? "configurationId")
            Core.<*> (x Core..:? "value")
            Core.<*> (x Core..:? "configurationType")
            Core.<*> (x Core..:? "timeOfCreation")
      )

instance Core.Hashable ConfigurationTag

instance Core.NFData ConfigurationTag
