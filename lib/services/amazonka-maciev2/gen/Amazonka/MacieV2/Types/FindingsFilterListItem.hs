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
-- Module      : Amazonka.MacieV2.Types.FindingsFilterListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingsFilterListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.FindingsFilterAction
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a findings filter.
--
-- /See:/ 'newFindingsFilterListItem' smart constructor.
data FindingsFilterListItem = FindingsFilterListItem'
  { -- | The action that\'s performed on findings that match the filter criteria.
    -- Possible values are: ARCHIVE, suppress (automatically archive) the
    -- findings; and, NOOP, don\'t perform any action on the findings.
    action :: Prelude.Maybe FindingsFilterAction,
    -- | The Amazon Resource Name (ARN) of the filter.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the filter.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that specifies which tags (keys and values) are
    -- associated with the filter.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingsFilterListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'findingsFilterListItem_action' - The action that\'s performed on findings that match the filter criteria.
-- Possible values are: ARCHIVE, suppress (automatically archive) the
-- findings; and, NOOP, don\'t perform any action on the findings.
--
-- 'arn', 'findingsFilterListItem_arn' - The Amazon Resource Name (ARN) of the filter.
--
-- 'id', 'findingsFilterListItem_id' - The unique identifier for the filter.
--
-- 'name', 'findingsFilterListItem_name' - The custom name of the filter.
--
-- 'tags', 'findingsFilterListItem_tags' - A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the filter.
newFindingsFilterListItem ::
  FindingsFilterListItem
newFindingsFilterListItem =
  FindingsFilterListItem'
    { action = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The action that\'s performed on findings that match the filter criteria.
-- Possible values are: ARCHIVE, suppress (automatically archive) the
-- findings; and, NOOP, don\'t perform any action on the findings.
findingsFilterListItem_action :: Lens.Lens' FindingsFilterListItem (Prelude.Maybe FindingsFilterAction)
findingsFilterListItem_action = Lens.lens (\FindingsFilterListItem' {action} -> action) (\s@FindingsFilterListItem' {} a -> s {action = a} :: FindingsFilterListItem)

-- | The Amazon Resource Name (ARN) of the filter.
findingsFilterListItem_arn :: Lens.Lens' FindingsFilterListItem (Prelude.Maybe Prelude.Text)
findingsFilterListItem_arn = Lens.lens (\FindingsFilterListItem' {arn} -> arn) (\s@FindingsFilterListItem' {} a -> s {arn = a} :: FindingsFilterListItem)

-- | The unique identifier for the filter.
findingsFilterListItem_id :: Lens.Lens' FindingsFilterListItem (Prelude.Maybe Prelude.Text)
findingsFilterListItem_id = Lens.lens (\FindingsFilterListItem' {id} -> id) (\s@FindingsFilterListItem' {} a -> s {id = a} :: FindingsFilterListItem)

-- | The custom name of the filter.
findingsFilterListItem_name :: Lens.Lens' FindingsFilterListItem (Prelude.Maybe Prelude.Text)
findingsFilterListItem_name = Lens.lens (\FindingsFilterListItem' {name} -> name) (\s@FindingsFilterListItem' {} a -> s {name = a} :: FindingsFilterListItem)

-- | A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the filter.
findingsFilterListItem_tags :: Lens.Lens' FindingsFilterListItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
findingsFilterListItem_tags = Lens.lens (\FindingsFilterListItem' {tags} -> tags) (\s@FindingsFilterListItem' {} a -> s {tags = a} :: FindingsFilterListItem) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FindingsFilterListItem where
  parseJSON =
    Data.withObject
      "FindingsFilterListItem"
      ( \x ->
          FindingsFilterListItem'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FindingsFilterListItem where
  hashWithSalt _salt FindingsFilterListItem' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData FindingsFilterListItem where
  rnf FindingsFilterListItem' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf tags
