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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.Tool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.Tool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of AWS services utilized in a migration workflow.
--
-- /See:/ 'newTool' smart constructor.
data Tool = Tool'
  { -- | The name of an AWS service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL of an AWS service.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'tool_name' - The name of an AWS service.
--
-- 'url', 'tool_url' - The URL of an AWS service.
newTool ::
  Tool
newTool =
  Tool'
    { name = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The name of an AWS service.
tool_name :: Lens.Lens' Tool (Prelude.Maybe Prelude.Text)
tool_name = Lens.lens (\Tool' {name} -> name) (\s@Tool' {} a -> s {name = a} :: Tool)

-- | The URL of an AWS service.
tool_url :: Lens.Lens' Tool (Prelude.Maybe Prelude.Text)
tool_url = Lens.lens (\Tool' {url} -> url) (\s@Tool' {} a -> s {url = a} :: Tool)

instance Data.FromJSON Tool where
  parseJSON =
    Data.withObject
      "Tool"
      ( \x ->
          Tool'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable Tool where
  hashWithSalt _salt Tool' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData Tool where
  rnf Tool' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf url
