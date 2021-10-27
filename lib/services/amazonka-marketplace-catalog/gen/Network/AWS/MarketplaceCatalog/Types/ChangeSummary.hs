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
-- Module      : Network.AWS.MarketplaceCatalog.Types.ChangeSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceCatalog.Types.ChangeSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceCatalog.Types.Entity
import Network.AWS.MarketplaceCatalog.Types.ErrorDetail
import qualified Network.AWS.Prelude as Prelude

-- | This object is a container for common summary information about the
-- change. The summary doesn\'t contain the whole change structure.
--
-- /See:/ 'newChangeSummary' smart constructor.
data ChangeSummary = ChangeSummary'
  { -- | Optional name for the change.
    changeName :: Prelude.Maybe Prelude.Text,
    -- | This object contains details specific to the change type of the
    -- requested change.
    details :: Prelude.Maybe Prelude.Text,
    -- | An array of @ErrorDetail@ objects associated with the change.
    errorDetailList :: Prelude.Maybe [ErrorDetail],
    -- | The entity to be changed.
    entity :: Prelude.Maybe Entity,
    -- | The type of the change.
    changeType :: Prelude.Maybe Prelude.Text
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
-- 'changeName', 'changeSummary_changeName' - Optional name for the change.
--
-- 'details', 'changeSummary_details' - This object contains details specific to the change type of the
-- requested change.
--
-- 'errorDetailList', 'changeSummary_errorDetailList' - An array of @ErrorDetail@ objects associated with the change.
--
-- 'entity', 'changeSummary_entity' - The entity to be changed.
--
-- 'changeType', 'changeSummary_changeType' - The type of the change.
newChangeSummary ::
  ChangeSummary
newChangeSummary =
  ChangeSummary'
    { changeName = Prelude.Nothing,
      details = Prelude.Nothing,
      errorDetailList = Prelude.Nothing,
      entity = Prelude.Nothing,
      changeType = Prelude.Nothing
    }

-- | Optional name for the change.
changeSummary_changeName :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_changeName = Lens.lens (\ChangeSummary' {changeName} -> changeName) (\s@ChangeSummary' {} a -> s {changeName = a} :: ChangeSummary)

-- | This object contains details specific to the change type of the
-- requested change.
changeSummary_details :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_details = Lens.lens (\ChangeSummary' {details} -> details) (\s@ChangeSummary' {} a -> s {details = a} :: ChangeSummary)

-- | An array of @ErrorDetail@ objects associated with the change.
changeSummary_errorDetailList :: Lens.Lens' ChangeSummary (Prelude.Maybe [ErrorDetail])
changeSummary_errorDetailList = Lens.lens (\ChangeSummary' {errorDetailList} -> errorDetailList) (\s@ChangeSummary' {} a -> s {errorDetailList = a} :: ChangeSummary) Prelude.. Lens.mapping Lens.coerced

-- | The entity to be changed.
changeSummary_entity :: Lens.Lens' ChangeSummary (Prelude.Maybe Entity)
changeSummary_entity = Lens.lens (\ChangeSummary' {entity} -> entity) (\s@ChangeSummary' {} a -> s {entity = a} :: ChangeSummary)

-- | The type of the change.
changeSummary_changeType :: Lens.Lens' ChangeSummary (Prelude.Maybe Prelude.Text)
changeSummary_changeType = Lens.lens (\ChangeSummary' {changeType} -> changeType) (\s@ChangeSummary' {} a -> s {changeType = a} :: ChangeSummary)

instance Core.FromJSON ChangeSummary where
  parseJSON =
    Core.withObject
      "ChangeSummary"
      ( \x ->
          ChangeSummary'
            Prelude.<$> (x Core..:? "ChangeName")
            Prelude.<*> (x Core..:? "Details")
            Prelude.<*> ( x Core..:? "ErrorDetailList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Entity")
            Prelude.<*> (x Core..:? "ChangeType")
      )

instance Prelude.Hashable ChangeSummary

instance Prelude.NFData ChangeSummary
