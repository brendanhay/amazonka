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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnect where

import Amazonka.Connect.Types.QuickConnectConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a quick connect.
--
-- /See:/ 'newQuickConnect' smart constructor.
data QuickConnect = QuickConnect'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the quick connect.
    name :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the quick connect.
    quickConnectConfig :: Prelude.Maybe QuickConnectConfig,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the quick connect.
    quickConnectARN :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'quickConnect_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'quickConnect_name' - The name of the quick connect.
--
-- 'quickConnectConfig', 'quickConnect_quickConnectConfig' - Contains information about the quick connect.
--
-- 'description', 'quickConnect_description' - The description.
--
-- 'quickConnectARN', 'quickConnect_quickConnectARN' - The Amazon Resource Name (ARN) of the quick connect.
--
-- 'quickConnectId', 'quickConnect_quickConnectId' - The identifier for the quick connect.
newQuickConnect ::
  QuickConnect
newQuickConnect =
  QuickConnect'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      quickConnectConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      quickConnectARN = Prelude.Nothing,
      quickConnectId = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
quickConnect_tags :: Lens.Lens' QuickConnect (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
quickConnect_tags = Lens.lens (\QuickConnect' {tags} -> tags) (\s@QuickConnect' {} a -> s {tags = a} :: QuickConnect) Prelude.. Lens.mapping Lens.coerced

-- | The name of the quick connect.
quickConnect_name :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_name = Lens.lens (\QuickConnect' {name} -> name) (\s@QuickConnect' {} a -> s {name = a} :: QuickConnect)

-- | Contains information about the quick connect.
quickConnect_quickConnectConfig :: Lens.Lens' QuickConnect (Prelude.Maybe QuickConnectConfig)
quickConnect_quickConnectConfig = Lens.lens (\QuickConnect' {quickConnectConfig} -> quickConnectConfig) (\s@QuickConnect' {} a -> s {quickConnectConfig = a} :: QuickConnect)

-- | The description.
quickConnect_description :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_description = Lens.lens (\QuickConnect' {description} -> description) (\s@QuickConnect' {} a -> s {description = a} :: QuickConnect)

-- | The Amazon Resource Name (ARN) of the quick connect.
quickConnect_quickConnectARN :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_quickConnectARN = Lens.lens (\QuickConnect' {quickConnectARN} -> quickConnectARN) (\s@QuickConnect' {} a -> s {quickConnectARN = a} :: QuickConnect)

-- | The identifier for the quick connect.
quickConnect_quickConnectId :: Lens.Lens' QuickConnect (Prelude.Maybe Prelude.Text)
quickConnect_quickConnectId = Lens.lens (\QuickConnect' {quickConnectId} -> quickConnectId) (\s@QuickConnect' {} a -> s {quickConnectId = a} :: QuickConnect)

instance Data.FromJSON QuickConnect where
  parseJSON =
    Data.withObject
      "QuickConnect"
      ( \x ->
          QuickConnect'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "QuickConnectConfig")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "QuickConnectARN")
            Prelude.<*> (x Data..:? "QuickConnectId")
      )

instance Prelude.Hashable QuickConnect where
  hashWithSalt _salt QuickConnect' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` quickConnectConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` quickConnectARN
      `Prelude.hashWithSalt` quickConnectId

instance Prelude.NFData QuickConnect where
  rnf QuickConnect' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf quickConnectConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf quickConnectARN
      `Prelude.seq` Prelude.rnf quickConnectId
