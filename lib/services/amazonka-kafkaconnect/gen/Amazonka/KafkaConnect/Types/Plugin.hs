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
-- Module      : Amazonka.KafkaConnect.Types.Plugin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.Plugin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.CustomPlugin
import qualified Amazonka.Prelude as Prelude

-- | A plugin is an AWS resource that contains the code that defines your
-- connector logic.
--
-- /See:/ 'newPlugin' smart constructor.
data Plugin = Plugin'
  { -- | Details about a custom plugin.
    customPlugin :: CustomPlugin
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Plugin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPlugin', 'plugin_customPlugin' - Details about a custom plugin.
newPlugin ::
  -- | 'customPlugin'
  CustomPlugin ->
  Plugin
newPlugin pCustomPlugin_ =
  Plugin' {customPlugin = pCustomPlugin_}

-- | Details about a custom plugin.
plugin_customPlugin :: Lens.Lens' Plugin CustomPlugin
plugin_customPlugin = Lens.lens (\Plugin' {customPlugin} -> customPlugin) (\s@Plugin' {} a -> s {customPlugin = a} :: Plugin)

instance Prelude.Hashable Plugin where
  hashWithSalt _salt Plugin' {..} =
    _salt `Prelude.hashWithSalt` customPlugin

instance Prelude.NFData Plugin where
  rnf Plugin' {..} = Prelude.rnf customPlugin

instance Data.ToJSON Plugin where
  toJSON Plugin' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("customPlugin" Data..= customPlugin)]
      )
