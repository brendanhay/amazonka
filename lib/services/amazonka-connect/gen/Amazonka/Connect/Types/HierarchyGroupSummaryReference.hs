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
-- Module      : Amazonka.Connect.Types.HierarchyGroupSummaryReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyGroupSummaryReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the hierarchy group.
--
-- /See:/ 'newHierarchyGroupSummaryReference' smart constructor.
data HierarchyGroupSummaryReference = HierarchyGroupSummaryReference'
  { -- | The Amazon Resource Name (ARN) for the hierarchy group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the hierarchy group.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyGroupSummaryReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'hierarchyGroupSummaryReference_arn' - The Amazon Resource Name (ARN) for the hierarchy group.
--
-- 'id', 'hierarchyGroupSummaryReference_id' - The unique identifier for the hierarchy group.
newHierarchyGroupSummaryReference ::
  HierarchyGroupSummaryReference
newHierarchyGroupSummaryReference =
  HierarchyGroupSummaryReference'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the hierarchy group.
hierarchyGroupSummaryReference_arn :: Lens.Lens' HierarchyGroupSummaryReference (Prelude.Maybe Prelude.Text)
hierarchyGroupSummaryReference_arn = Lens.lens (\HierarchyGroupSummaryReference' {arn} -> arn) (\s@HierarchyGroupSummaryReference' {} a -> s {arn = a} :: HierarchyGroupSummaryReference)

-- | The unique identifier for the hierarchy group.
hierarchyGroupSummaryReference_id :: Lens.Lens' HierarchyGroupSummaryReference (Prelude.Maybe Prelude.Text)
hierarchyGroupSummaryReference_id = Lens.lens (\HierarchyGroupSummaryReference' {id} -> id) (\s@HierarchyGroupSummaryReference' {} a -> s {id = a} :: HierarchyGroupSummaryReference)

instance Core.FromJSON HierarchyGroupSummaryReference where
  parseJSON =
    Core.withObject
      "HierarchyGroupSummaryReference"
      ( \x ->
          HierarchyGroupSummaryReference'
            Prelude.<$> (x Core..:? "Arn") Prelude.<*> (x Core..:? "Id")
      )

instance
  Prelude.Hashable
    HierarchyGroupSummaryReference
  where
  hashWithSalt
    _salt
    HierarchyGroupSummaryReference' {..} =
      _salt `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    HierarchyGroupSummaryReference
  where
  rnf HierarchyGroupSummaryReference' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
