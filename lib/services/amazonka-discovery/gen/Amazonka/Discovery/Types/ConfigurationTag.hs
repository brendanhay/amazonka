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
-- Module      : Amazonka.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ConfigurationTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.ConfigurationItemType
import qualified Amazonka.Prelude as Prelude

-- | Tags for a configuration item. Tags are metadata that help you
-- categorize IT assets.
--
-- /See:/ 'newConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { -- | The configuration ID for the item to tag. You can specify a list of keys
    -- and values.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | A type of IT asset to tag.
    configurationType :: Prelude.Maybe ConfigurationItemType,
    -- | A type of tag on which to filter. For example, /serverType/.
    key :: Prelude.Maybe Prelude.Text,
    -- | The time the configuration tag was created in Coordinated Universal Time
    -- (UTC).
    timeOfCreation :: Prelude.Maybe Data.POSIX,
    -- | A value on which to filter. For example /key = serverType/ and /value =
    -- web server/.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationId', 'configurationTag_configurationId' - The configuration ID for the item to tag. You can specify a list of keys
-- and values.
--
-- 'configurationType', 'configurationTag_configurationType' - A type of IT asset to tag.
--
-- 'key', 'configurationTag_key' - A type of tag on which to filter. For example, /serverType/.
--
-- 'timeOfCreation', 'configurationTag_timeOfCreation' - The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
--
-- 'value', 'configurationTag_value' - A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
newConfigurationTag ::
  ConfigurationTag
newConfigurationTag =
  ConfigurationTag'
    { configurationId =
        Prelude.Nothing,
      configurationType = Prelude.Nothing,
      key = Prelude.Nothing,
      timeOfCreation = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The configuration ID for the item to tag. You can specify a list of keys
-- and values.
configurationTag_configurationId :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_configurationId = Lens.lens (\ConfigurationTag' {configurationId} -> configurationId) (\s@ConfigurationTag' {} a -> s {configurationId = a} :: ConfigurationTag)

-- | A type of IT asset to tag.
configurationTag_configurationType :: Lens.Lens' ConfigurationTag (Prelude.Maybe ConfigurationItemType)
configurationTag_configurationType = Lens.lens (\ConfigurationTag' {configurationType} -> configurationType) (\s@ConfigurationTag' {} a -> s {configurationType = a} :: ConfigurationTag)

-- | A type of tag on which to filter. For example, /serverType/.
configurationTag_key :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_key = Lens.lens (\ConfigurationTag' {key} -> key) (\s@ConfigurationTag' {} a -> s {key = a} :: ConfigurationTag)

-- | The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
configurationTag_timeOfCreation :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.UTCTime)
configurationTag_timeOfCreation = Lens.lens (\ConfigurationTag' {timeOfCreation} -> timeOfCreation) (\s@ConfigurationTag' {} a -> s {timeOfCreation = a} :: ConfigurationTag) Prelude.. Lens.mapping Data._Time

-- | A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
configurationTag_value :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_value = Lens.lens (\ConfigurationTag' {value} -> value) (\s@ConfigurationTag' {} a -> s {value = a} :: ConfigurationTag)

instance Data.FromJSON ConfigurationTag where
  parseJSON =
    Data.withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            Prelude.<$> (x Data..:? "configurationId")
            Prelude.<*> (x Data..:? "configurationType")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "timeOfCreation")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ConfigurationTag where
  hashWithSalt _salt ConfigurationTag' {..} =
    _salt `Prelude.hashWithSalt` configurationId
      `Prelude.hashWithSalt` configurationType
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` timeOfCreation
      `Prelude.hashWithSalt` value

instance Prelude.NFData ConfigurationTag where
  rnf ConfigurationTag' {..} =
    Prelude.rnf configurationId
      `Prelude.seq` Prelude.rnf configurationType
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf timeOfCreation
      `Prelude.seq` Prelude.rnf value
