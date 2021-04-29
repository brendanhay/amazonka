{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.PatchFilterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilterGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.PatchFilter

-- | A set of patch filters, typically used for approval rules.
--
-- /See:/ 'newPatchFilterGroup' smart constructor.
data PatchFilterGroup = PatchFilterGroup'
  { -- | The set of patch filters that make up the group.
    patchFilters :: [PatchFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
patchFilterGroup_patchFilters = Lens.lens (\PatchFilterGroup' {patchFilters} -> patchFilters) (\s@PatchFilterGroup' {} a -> s {patchFilters = a} :: PatchFilterGroup) Prelude.. Prelude._Coerce

instance Prelude.FromJSON PatchFilterGroup where
  parseJSON =
    Prelude.withObject
      "PatchFilterGroup"
      ( \x ->
          PatchFilterGroup'
            Prelude.<$> ( x Prelude..:? "PatchFilters"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PatchFilterGroup

instance Prelude.NFData PatchFilterGroup

instance Prelude.ToJSON PatchFilterGroup where
  toJSON PatchFilterGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PatchFilters" Prelude..= patchFilters)
          ]
      )
