{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Discovery.Types.ConfigurationItemType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Tags for a configuration item. Tags are metadata that help you
-- categorize IT assets.
--
-- /See:/ 'newConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { -- | A type of tag on which to filter. For example, /serverType/.
    key :: Prelude.Maybe Prelude.Text,
    -- | The configuration ID for the item to tag. You can specify a list of keys
    -- and values.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | A value on which to filter. For example /key = serverType/ and /value =
    -- web server/.
    value :: Prelude.Maybe Prelude.Text,
    -- | A type of IT asset to tag.
    configurationType :: Prelude.Maybe ConfigurationItemType,
    -- | The time the configuration tag was created in Coordinated Universal Time
    -- (UTC).
    timeOfCreation :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      configurationId = Prelude.Nothing,
      value = Prelude.Nothing,
      configurationType = Prelude.Nothing,
      timeOfCreation = Prelude.Nothing
    }

-- | A type of tag on which to filter. For example, /serverType/.
configurationTag_key :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_key = Lens.lens (\ConfigurationTag' {key} -> key) (\s@ConfigurationTag' {} a -> s {key = a} :: ConfigurationTag)

-- | The configuration ID for the item to tag. You can specify a list of keys
-- and values.
configurationTag_configurationId :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_configurationId = Lens.lens (\ConfigurationTag' {configurationId} -> configurationId) (\s@ConfigurationTag' {} a -> s {configurationId = a} :: ConfigurationTag)

-- | A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
configurationTag_value :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_value = Lens.lens (\ConfigurationTag' {value} -> value) (\s@ConfigurationTag' {} a -> s {value = a} :: ConfigurationTag)

-- | A type of IT asset to tag.
configurationTag_configurationType :: Lens.Lens' ConfigurationTag (Prelude.Maybe ConfigurationItemType)
configurationTag_configurationType = Lens.lens (\ConfigurationTag' {configurationType} -> configurationType) (\s@ConfigurationTag' {} a -> s {configurationType = a} :: ConfigurationTag)

-- | The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
configurationTag_timeOfCreation :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.UTCTime)
configurationTag_timeOfCreation = Lens.lens (\ConfigurationTag' {timeOfCreation} -> timeOfCreation) (\s@ConfigurationTag' {} a -> s {timeOfCreation = a} :: ConfigurationTag) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ConfigurationTag where
  parseJSON =
    Prelude.withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            Prelude.<$> (x Prelude..:? "key")
            Prelude.<*> (x Prelude..:? "configurationId")
            Prelude.<*> (x Prelude..:? "value")
            Prelude.<*> (x Prelude..:? "configurationType")
            Prelude.<*> (x Prelude..:? "timeOfCreation")
      )

instance Prelude.Hashable ConfigurationTag

instance Prelude.NFData ConfigurationTag
