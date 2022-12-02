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
-- Module      : Amazonka.SSM.Types.PatchFilterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchFilterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.PatchFilter

-- | A set of patch filters, typically used for approval rules.
--
-- /See:/ 'newPatchFilterGroup' smart constructor.
data PatchFilterGroup = PatchFilterGroup'
  { -- | The set of patch filters that make up the group.
    patchFilters :: [PatchFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchFilterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchFilters', 'patchFilterGroup_patchFilters' - The set of patch filters that make up the group.
newPatchFilterGroup ::
  PatchFilterGroup
newPatchFilterGroup =
  PatchFilterGroup' {patchFilters = Prelude.mempty}

-- | The set of patch filters that make up the group.
patchFilterGroup_patchFilters :: Lens.Lens' PatchFilterGroup [PatchFilter]
patchFilterGroup_patchFilters = Lens.lens (\PatchFilterGroup' {patchFilters} -> patchFilters) (\s@PatchFilterGroup' {} a -> s {patchFilters = a} :: PatchFilterGroup) Prelude.. Lens.coerced

instance Data.FromJSON PatchFilterGroup where
  parseJSON =
    Data.withObject
      "PatchFilterGroup"
      ( \x ->
          PatchFilterGroup'
            Prelude.<$> (x Data..:? "PatchFilters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PatchFilterGroup where
  hashWithSalt _salt PatchFilterGroup' {..} =
    _salt `Prelude.hashWithSalt` patchFilters

instance Prelude.NFData PatchFilterGroup where
  rnf PatchFilterGroup' {..} = Prelude.rnf patchFilters

instance Data.ToJSON PatchFilterGroup where
  toJSON PatchFilterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PatchFilters" Data..= patchFilters)]
      )
