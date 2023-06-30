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
-- Module      : Amazonka.Connect.Types.QuickConnectSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnectSummary where

import Amazonka.Connect.Types.QuickConnectType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a quick connect.
--
-- /See:/ 'newQuickConnectSummary' smart constructor.
data QuickConnectSummary = QuickConnectSummary'
  { -- | The Amazon Resource Name (ARN) of the quick connect.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the quick connect.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the quick connect.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of quick connect. In the Amazon Connect console, when you
    -- create a quick connect, you are prompted to assign one of the following
    -- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
    quickConnectType :: Prelude.Maybe QuickConnectType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuickConnectSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'quickConnectSummary_arn' - The Amazon Resource Name (ARN) of the quick connect.
--
-- 'id', 'quickConnectSummary_id' - The identifier for the quick connect.
--
-- 'name', 'quickConnectSummary_name' - The name of the quick connect.
--
-- 'quickConnectType', 'quickConnectSummary_quickConnectType' - The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
newQuickConnectSummary ::
  QuickConnectSummary
newQuickConnectSummary =
  QuickConnectSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      quickConnectType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the quick connect.
quickConnectSummary_arn :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_arn = Lens.lens (\QuickConnectSummary' {arn} -> arn) (\s@QuickConnectSummary' {} a -> s {arn = a} :: QuickConnectSummary)

-- | The identifier for the quick connect.
quickConnectSummary_id :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_id = Lens.lens (\QuickConnectSummary' {id} -> id) (\s@QuickConnectSummary' {} a -> s {id = a} :: QuickConnectSummary)

-- | The name of the quick connect.
quickConnectSummary_name :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_name = Lens.lens (\QuickConnectSummary' {name} -> name) (\s@QuickConnectSummary' {} a -> s {name = a} :: QuickConnectSummary)

-- | The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
quickConnectSummary_quickConnectType :: Lens.Lens' QuickConnectSummary (Prelude.Maybe QuickConnectType)
quickConnectSummary_quickConnectType = Lens.lens (\QuickConnectSummary' {quickConnectType} -> quickConnectType) (\s@QuickConnectSummary' {} a -> s {quickConnectType = a} :: QuickConnectSummary)

instance Data.FromJSON QuickConnectSummary where
  parseJSON =
    Data.withObject
      "QuickConnectSummary"
      ( \x ->
          QuickConnectSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "QuickConnectType")
      )

instance Prelude.Hashable QuickConnectSummary where
  hashWithSalt _salt QuickConnectSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` quickConnectType

instance Prelude.NFData QuickConnectSummary where
  rnf QuickConnectSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf quickConnectType
