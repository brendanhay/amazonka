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
-- Module      : Amazonka.Connect.Types.QuickConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnect where

import Amazonka.Connect.Types.QuickConnectConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a quick connect.
--
-- /See:/ 'newQuickConnect' smart constructor.
data QuickConnect = QuickConnect'
  { -- | The name of the quick connect.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the quick connect.
    quickConnectARN :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about the quick connect.
    quickConnectConfig :: Prelude.Maybe QuickConnectConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'quickConnect_name' - The name of the quick connect.
--
-- 'quickConnectId', 'quickConnect_quickConnectId' - The identifier for the quick connect.
--
-- 'description', 'quickConnect_description' - The description.
--
-- 'quickConnectARN', 'quickConnect_quickConnectARN' - The Amazon Resource Name (ARN) of the quick connect.
--
-- 'tags', 'quickConnect_tags' - One or more tags.
--
-- 'quickConnectConfig', 'quickConnect_quickConnectConfig' - Contains information about the quick connect.
newQuickConnect ::
  QuickConnect
newQuickConnect =
  QuickConnect'
    { name = Prelude.Nothing,
      quickConnectId = Prelude.Nothing,
      description = Prelude.Nothing,
      quickConnectARN = Prelude.Nothing,
      tags = Prelude.Nothing,
      quickConnectConfig = Prelude.Nothing
    }

-- | The name of the quick connect.
quickConnect_name :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_name = Lens.lens (\QuickConnect' {name} -> name) (\s@QuickConnect' {} a -> s {name = a} :: QuickConnect)

-- | The identifier for the quick connect.
quickConnect_quickConnectId :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_quickConnectId = Lens.lens (\QuickConnect' {quickConnectId} -> quickConnectId) (\s@QuickConnect' {} a -> s {quickConnectId = a} :: QuickConnect)

-- | The description.
quickConnect_description :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_description = Lens.lens (\QuickConnect' {description} -> description) (\s@QuickConnect' {} a -> s {description = a} :: QuickConnect)

-- | The Amazon Resource Name (ARN) of the quick connect.
quickConnect_quickConnectARN :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_quickConnectARN = Lens.lens (\QuickConnect' {quickConnectARN} -> quickConnectARN) (\s@QuickConnect' {} a -> s {quickConnectARN = a} :: QuickConnect)

-- | One or more tags.
quickConnect_tags :: Lens.Lens' QuickConnect (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
quickConnect_tags = Lens.lens (\QuickConnect' {tags} -> tags) (\s@QuickConnect' {} a -> s {tags = a} :: QuickConnect) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the quick connect.
quickConnect_quickConnectConfig :: Lens.Lens' QuickConnect (Prelude.Maybe QuickConnectConfig)
quickConnect_quickConnectConfig = Lens.lens (\QuickConnect' {quickConnectConfig} -> quickConnectConfig) (\s@QuickConnect' {} a -> s {quickConnectConfig = a} :: QuickConnect)

instance Core.FromJSON QuickConnect where
  parseJSON =
    Core.withObject
      "QuickConnect"
      ( \x ->
          QuickConnect'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "QuickConnectId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "QuickConnectARN")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "QuickConnectConfig")
      )

instance Prelude.Hashable QuickConnect where
  hashWithSalt _salt QuickConnect' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` quickConnectId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` quickConnectARN
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` quickConnectConfig

instance Prelude.NFData QuickConnect where
  rnf QuickConnect' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf quickConnectId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf quickConnectARN
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf quickConnectConfig
