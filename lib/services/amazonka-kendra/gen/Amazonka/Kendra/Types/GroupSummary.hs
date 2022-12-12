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
-- Module      : Amazonka.Kendra.Types.GroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.GroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information for groups.
--
-- /See:/ 'newGroupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { -- | The identifier of the group you want group summary information on.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp identifier used for the latest @PUT@ or @DELETE@ action.
    orderingId :: Prelude.Maybe Prelude.Natural
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
-- 'groupId', 'groupSummary_groupId' - The identifier of the group you want group summary information on.
--
-- 'orderingId', 'groupSummary_orderingId' - The timestamp identifier used for the latest @PUT@ or @DELETE@ action.
newGroupSummary ::
  GroupSummary
newGroupSummary =
  GroupSummary'
    { groupId = Prelude.Nothing,
      orderingId = Prelude.Nothing
    }

-- | The identifier of the group you want group summary information on.
groupSummary_groupId :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_groupId = Lens.lens (\GroupSummary' {groupId} -> groupId) (\s@GroupSummary' {} a -> s {groupId = a} :: GroupSummary)

-- | The timestamp identifier used for the latest @PUT@ or @DELETE@ action.
groupSummary_orderingId :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Natural)
groupSummary_orderingId = Lens.lens (\GroupSummary' {orderingId} -> orderingId) (\s@GroupSummary' {} a -> s {orderingId = a} :: GroupSummary)

instance Data.FromJSON GroupSummary where
  parseJSON =
    Data.withObject
      "GroupSummary"
      ( \x ->
          GroupSummary'
            Prelude.<$> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "OrderingId")
      )

instance Prelude.Hashable GroupSummary where
  hashWithSalt _salt GroupSummary' {..} =
    _salt `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` orderingId

instance Prelude.NFData GroupSummary where
  rnf GroupSummary' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf orderingId
