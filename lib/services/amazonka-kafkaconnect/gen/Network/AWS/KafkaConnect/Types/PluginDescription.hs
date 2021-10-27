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
-- Module      : Network.AWS.KafkaConnect.Types.PluginDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.PluginDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types.CustomPluginDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON PluginDescription where
  parseJSON =
    Core.withObject
      "PluginDescription"
      ( \x ->
          PluginDescription'
            Prelude.<$> (x Core..:? "customPlugin")
      )

instance Prelude.Hashable PluginDescription

instance Prelude.NFData PluginDescription
