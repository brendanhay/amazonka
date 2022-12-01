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
-- Module      : Amazonka.Synthetics.Types.GroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.GroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing some information about a group.
--
-- /See:/ 'newGroupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the group.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'groupSummary_name' - The name of the group.
--
-- 'arn', 'groupSummary_arn' - The ARN of the group.
--
-- 'id', 'groupSummary_id' - The unique ID of the group.
newGroupSummary ::
  GroupSummary
newGroupSummary =
  GroupSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The name of the group.
groupSummary_name :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_name = Lens.lens (\GroupSummary' {name} -> name) (\s@GroupSummary' {} a -> s {name = a} :: GroupSummary)

-- | The ARN of the group.
groupSummary_arn :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_arn = Lens.lens (\GroupSummary' {arn} -> arn) (\s@GroupSummary' {} a -> s {arn = a} :: GroupSummary)

-- | The unique ID of the group.
groupSummary_id :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_id = Lens.lens (\GroupSummary' {id} -> id) (\s@GroupSummary' {} a -> s {id = a} :: GroupSummary)

instance Core.FromJSON GroupSummary where
  parseJSON =
    Core.withObject
      "GroupSummary"
      ( \x ->
          GroupSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable GroupSummary where
  hashWithSalt _salt GroupSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData GroupSummary where
  rnf GroupSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
