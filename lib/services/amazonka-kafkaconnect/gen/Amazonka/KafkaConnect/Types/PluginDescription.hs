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
-- Module      : Amazonka.KafkaConnect.Types.PluginDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.PluginDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.CustomPluginDescription
import qualified Amazonka.Prelude as Prelude

-- | The description of the plugin.
--
-- /See:/ 'newPluginDescription' smart constructor.
data PluginDescription = PluginDescription'
  { -- | Details about a custom plugin.
    customPlugin :: Prelude.Maybe CustomPluginDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PluginDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPlugin', 'pluginDescription_customPlugin' - Details about a custom plugin.
newPluginDescription ::
  PluginDescription
newPluginDescription =
  PluginDescription' {customPlugin = Prelude.Nothing}

-- | Details about a custom plugin.
pluginDescription_customPlugin :: Lens.Lens' PluginDescription (Prelude.Maybe CustomPluginDescription)
pluginDescription_customPlugin = Lens.lens (\PluginDescription' {customPlugin} -> customPlugin) (\s@PluginDescription' {} a -> s {customPlugin = a} :: PluginDescription)

instance Data.FromJSON PluginDescription where
  parseJSON =
    Data.withObject
      "PluginDescription"
      ( \x ->
          PluginDescription'
            Prelude.<$> (x Data..:? "customPlugin")
      )

instance Prelude.Hashable PluginDescription where
  hashWithSalt _salt PluginDescription' {..} =
    _salt `Prelude.hashWithSalt` customPlugin

instance Prelude.NFData PluginDescription where
  rnf PluginDescription' {..} = Prelude.rnf customPlugin
