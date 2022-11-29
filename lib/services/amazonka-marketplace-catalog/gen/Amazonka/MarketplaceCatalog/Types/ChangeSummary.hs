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
-- Module      : Amazonka.MarketplaceCatalog.Types.ChangeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.ChangeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceCatalog.Types.Entity
import Amazonka.MarketplaceCatalog.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | This object is a container for common summary information about the
-- change. The summary doesn\'t contain the whole change structure.
--
-- /See:/ 'newChangeSummary' smart constructor.
data ChangeSummary = ChangeSummary'
  { -- | The entity to be changed.
    entity :: Prelude.Maybe Entity,
    -- | Optional name for the change.
    changeName :: Prelude.Maybe Prelude.Text,
    -- | The type of the change.
    changeType :: Prelude.Maybe Prelude.Text,
    -- | This object contains details specific to the change type of the
    -- requested change.
    details :: Prelude.Maybe Prelude.Text,
    -- | An array of @ErrorDetail@ objects associated with the change.
    errorDetailList :: Prelude.Maybe [ErrorDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'changeSummary_entity' - The entity to be changed.
--
-- 'changeName', 'changeSummary_changeName' - Optional name for the change.
--
-- 'changeType', 'changeSummary_changeType' - The type of the change.
--
-- 'details', 'changeSummary_details' - This object contains details specific to the change type of the
-- requested change.
--
-- 'errorDetailList', 'changeSummary_errorDetailList' - An array of @ErrorDetail@ objects associated with the change.
newChangeSummary ::
  ChangeSummary
newChangeSummary =
  ChangeSummary'
    { entity = Prelude.Nothing,
      changeName = Prelude.Nothing,
      changeType = Prelude.Nothing,
      details = Prelude.Nothing,
      errorDetailList = Prelude.Nothing
    }

-- | The entity to be changed.
changeSummary_entity :: Lens.Lens' ChangeSummary (Prelude.Maybe Entity)
changeSummary_entity = Lens.lens (\ChangeSummary' {entity} -> entity) (\s@ChangeSummary' {} a -> s {entity = a} :: ChangeSummary)

-- | Optional name for the change.
changeSummary_changeName :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_changeName = Lens.lens (\ChangeSummary' {changeName} -> changeName) (\s@ChangeSummary' {} a -> s {changeName = a} :: ChangeSummary)

-- | The type of the change.
changeSummary_changeType :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_changeType = Lens.lens (\ChangeSummary' {changeType} -> changeType) (\s@ChangeSummary' {} a -> s {changeType = a} :: ChangeSummary)

-- | This object contains details specific to the change type of the
-- requested change.
changeSummary_details :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_details = Lens.lens (\ChangeSummary' {details} -> details) (\s@ChangeSummary' {} a -> s {details = a} :: ChangeSummary)

-- | An array of @ErrorDetail@ objects associated with the change.
changeSummary_errorDetailList :: Lens.Lens' ChangeSummary (Prelude.Maybe [ErrorDetail])
changeSummary_errorDetailList = Lens.lens (\ChangeSummary' {errorDetailList} -> errorDetailList) (\s@ChangeSummary' {} a -> s {errorDetailList = a} :: ChangeSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ChangeSummary where
  parseJSON =
    Core.withObject
      "ChangeSummary"
      ( \x ->
          ChangeSummary'
            Prelude.<$> (x Core..:? "Entity")
            Prelude.<*> (x Core..:? "ChangeName")
            Prelude.<*> (x Core..:? "ChangeType")
            Prelude.<*> (x Core..:? "Details")
            Prelude.<*> ( x Core..:? "ErrorDetailList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ChangeSummary where
  hashWithSalt _salt ChangeSummary' {..} =
    _salt `Prelude.hashWithSalt` entity
      `Prelude.hashWithSalt` changeName
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` errorDetailList

instance Prelude.NFData ChangeSummary where
  rnf ChangeSummary' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf changeName
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf errorDetailList
