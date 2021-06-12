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
-- Module      : Network.AWS.Connect.Types.QuickConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QuickConnect where

import Network.AWS.Connect.Types.QuickConnectConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a quick connect.
--
-- /See:/ 'newQuickConnect' smart constructor.
data QuickConnect = QuickConnect'
  { -- | The identifier for the quick connect.
    quickConnectId :: Core.Maybe Core.Text,
    -- | The name of the quick connect.
    name :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Contains information about the quick connect.
    quickConnectConfig :: Core.Maybe QuickConnectConfig,
    -- | The Amazon Resource Name (ARN) of the quick connect.
    quickConnectARN :: Core.Maybe Core.Text,
    -- | The description.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quickConnectId', 'quickConnect_quickConnectId' - The identifier for the quick connect.
--
-- 'name', 'quickConnect_name' - The name of the quick connect.
--
-- 'tags', 'quickConnect_tags' - One or more tags.
--
-- 'quickConnectConfig', 'quickConnect_quickConnectConfig' - Contains information about the quick connect.
--
-- 'quickConnectARN', 'quickConnect_quickConnectARN' - The Amazon Resource Name (ARN) of the quick connect.
--
-- 'description', 'quickConnect_description' - The description.
newQuickConnect ::
  QuickConnect
newQuickConnect =
  QuickConnect'
    { quickConnectId = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      quickConnectConfig = Core.Nothing,
      quickConnectARN = Core.Nothing,
      description = Core.Nothing
    }

-- | The identifier for the quick connect.
quickConnect_quickConnectId :: Lens.Lens' QuickConnect (Core.Maybe Core.Text)
quickConnect_quickConnectId = Lens.lens (\QuickConnect' {quickConnectId} -> quickConnectId) (\s@QuickConnect' {} a -> s {quickConnectId = a} :: QuickConnect)

-- | The name of the quick connect.
quickConnect_name :: Lens.Lens' QuickConnect (Core.Maybe Core.Text)
quickConnect_name = Lens.lens (\QuickConnect' {name} -> name) (\s@QuickConnect' {} a -> s {name = a} :: QuickConnect)

-- | One or more tags.
quickConnect_tags :: Lens.Lens' QuickConnect (Core.Maybe (Core.HashMap Core.Text Core.Text))
quickConnect_tags = Lens.lens (\QuickConnect' {tags} -> tags) (\s@QuickConnect' {} a -> s {tags = a} :: QuickConnect) Core.. Lens.mapping Lens._Coerce

-- | Contains information about the quick connect.
quickConnect_quickConnectConfig :: Lens.Lens' QuickConnect (Core.Maybe QuickConnectConfig)
quickConnect_quickConnectConfig = Lens.lens (\QuickConnect' {quickConnectConfig} -> quickConnectConfig) (\s@QuickConnect' {} a -> s {quickConnectConfig = a} :: QuickConnect)

-- | The Amazon Resource Name (ARN) of the quick connect.
quickConnect_quickConnectARN :: Lens.Lens' QuickConnect (Core.Maybe Core.Text)
quickConnect_quickConnectARN = Lens.lens (\QuickConnect' {quickConnectARN} -> quickConnectARN) (\s@QuickConnect' {} a -> s {quickConnectARN = a} :: QuickConnect)

-- | The description.
quickConnect_description :: Lens.Lens' QuickConnect (Core.Maybe Core.Text)
quickConnect_description = Lens.lens (\QuickConnect' {description} -> description) (\s@QuickConnect' {} a -> s {description = a} :: QuickConnect)

instance Core.FromJSON QuickConnect where
  parseJSON =
    Core.withObject
      "QuickConnect"
      ( \x ->
          QuickConnect'
            Core.<$> (x Core..:? "QuickConnectId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "QuickConnectConfig")
            Core.<*> (x Core..:? "QuickConnectARN")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable QuickConnect

instance Core.NFData QuickConnect
