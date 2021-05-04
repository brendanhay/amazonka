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
-- Module      : Network.AWS.SSM.Types.PatchRuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchRuleGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.PatchRule

-- | A set of rules defining the approval rules for a patch baseline.
--
-- /See:/ 'newPatchRuleGroup' smart constructor.
data PatchRuleGroup = PatchRuleGroup'
  { -- | The rules that make up the rule group.
    patchRules :: [PatchRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PatchRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchRules', 'patchRuleGroup_patchRules' - The rules that make up the rule group.
newPatchRuleGroup ::
  PatchRuleGroup
newPatchRuleGroup =
  PatchRuleGroup' {patchRules = Prelude.mempty}

-- | The rules that make up the rule group.
patchRuleGroup_patchRules :: Lens.Lens' PatchRuleGroup [PatchRule]
patchRuleGroup_patchRules = Lens.lens (\PatchRuleGroup' {patchRules} -> patchRules) (\s@PatchRuleGroup' {} a -> s {patchRules = a} :: PatchRuleGroup) Prelude.. Prelude._Coerce

instance Prelude.FromJSON PatchRuleGroup where
  parseJSON =
    Prelude.withObject
      "PatchRuleGroup"
      ( \x ->
          PatchRuleGroup'
            Prelude.<$> ( x Prelude..:? "PatchRules"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PatchRuleGroup

instance Prelude.NFData PatchRuleGroup

instance Prelude.ToJSON PatchRuleGroup where
  toJSON PatchRuleGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("PatchRules" Prelude..= patchRules)]
      )
