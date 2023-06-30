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
-- Module      : Amazonka.KafkaConnect.Types.CustomPlugin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CustomPlugin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A plugin is an AWS resource that contains the code that defines a
-- connector\'s logic.
--
-- /See:/ 'newCustomPlugin' smart constructor.
data CustomPlugin = CustomPlugin'
  { -- | The Amazon Resource Name (ARN) of the custom plugin.
    customPluginArn :: Prelude.Text,
    -- | The revision of the custom plugin.
    revision :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPlugin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPluginArn', 'customPlugin_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin.
--
-- 'revision', 'customPlugin_revision' - The revision of the custom plugin.
newCustomPlugin ::
  -- | 'customPluginArn'
  Prelude.Text ->
  -- | 'revision'
  Prelude.Natural ->
  CustomPlugin
newCustomPlugin pCustomPluginArn_ pRevision_ =
  CustomPlugin'
    { customPluginArn = pCustomPluginArn_,
      revision = pRevision_
    }

-- | The Amazon Resource Name (ARN) of the custom plugin.
customPlugin_customPluginArn :: Lens.Lens' CustomPlugin Prelude.Text
customPlugin_customPluginArn = Lens.lens (\CustomPlugin' {customPluginArn} -> customPluginArn) (\s@CustomPlugin' {} a -> s {customPluginArn = a} :: CustomPlugin)

-- | The revision of the custom plugin.
customPlugin_revision :: Lens.Lens' CustomPlugin Prelude.Natural
customPlugin_revision = Lens.lens (\CustomPlugin' {revision} -> revision) (\s@CustomPlugin' {} a -> s {revision = a} :: CustomPlugin)

instance Prelude.Hashable CustomPlugin where
  hashWithSalt _salt CustomPlugin' {..} =
    _salt
      `Prelude.hashWithSalt` customPluginArn
      `Prelude.hashWithSalt` revision

instance Prelude.NFData CustomPlugin where
  rnf CustomPlugin' {..} =
    Prelude.rnf customPluginArn
      `Prelude.seq` Prelude.rnf revision

instance Data.ToJSON CustomPlugin where
  toJSON CustomPlugin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("customPluginArn" Data..= customPluginArn),
            Prelude.Just ("revision" Data..= revision)
          ]
      )
