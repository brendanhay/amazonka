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
-- Module      : Amazonka.OAM.Types.ListSinksItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OAM.Types.ListSinksItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one of this monitoring
-- account\'s sinks.
--
-- /See:/ 'newListSinksItem' smart constructor.
data ListSinksItem = ListSinksItem'
  { -- | The ARN of the sink.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- sink ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the sink.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSinksItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listSinksItem_arn' - The ARN of the sink.
--
-- 'id', 'listSinksItem_id' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'name', 'listSinksItem_name' - The name of the sink.
newListSinksItem ::
  ListSinksItem
newListSinksItem =
  ListSinksItem'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the sink.
listSinksItem_arn :: Lens.Lens' ListSinksItem (Prelude.Maybe Prelude.Text)
listSinksItem_arn = Lens.lens (\ListSinksItem' {arn} -> arn) (\s@ListSinksItem' {} a -> s {arn = a} :: ListSinksItem)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
listSinksItem_id :: Lens.Lens' ListSinksItem (Prelude.Maybe Prelude.Text)
listSinksItem_id = Lens.lens (\ListSinksItem' {id} -> id) (\s@ListSinksItem' {} a -> s {id = a} :: ListSinksItem)

-- | The name of the sink.
listSinksItem_name :: Lens.Lens' ListSinksItem (Prelude.Maybe Prelude.Text)
listSinksItem_name = Lens.lens (\ListSinksItem' {name} -> name) (\s@ListSinksItem' {} a -> s {name = a} :: ListSinksItem)

instance Data.FromJSON ListSinksItem where
  parseJSON =
    Data.withObject
      "ListSinksItem"
      ( \x ->
          ListSinksItem'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ListSinksItem where
  hashWithSalt _salt ListSinksItem' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListSinksItem where
  rnf ListSinksItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
