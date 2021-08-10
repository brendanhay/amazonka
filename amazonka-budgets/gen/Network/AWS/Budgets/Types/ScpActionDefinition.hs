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
-- Module      : Network.AWS.Budgets.Types.ScpActionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ScpActionDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The service control policies (SCP) action definition details.
--
-- /See:/ 'newScpActionDefinition' smart constructor.
data ScpActionDefinition = ScpActionDefinition'
  { -- | The policy ID attached.
    policyId :: Prelude.Text,
    -- | A list of target IDs.
    targetIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScpActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'scpActionDefinition_policyId' - The policy ID attached.
--
-- 'targetIds', 'scpActionDefinition_targetIds' - A list of target IDs.
newScpActionDefinition ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'targetIds'
  Prelude.NonEmpty Prelude.Text ->
  ScpActionDefinition
newScpActionDefinition pPolicyId_ pTargetIds_ =
  ScpActionDefinition'
    { policyId = pPolicyId_,
      targetIds = Lens._Coerce Lens.# pTargetIds_
    }

-- | The policy ID attached.
scpActionDefinition_policyId :: Lens.Lens' ScpActionDefinition Prelude.Text
scpActionDefinition_policyId = Lens.lens (\ScpActionDefinition' {policyId} -> policyId) (\s@ScpActionDefinition' {} a -> s {policyId = a} :: ScpActionDefinition)

-- | A list of target IDs.
scpActionDefinition_targetIds :: Lens.Lens' ScpActionDefinition (Prelude.NonEmpty Prelude.Text)
scpActionDefinition_targetIds = Lens.lens (\ScpActionDefinition' {targetIds} -> targetIds) (\s@ScpActionDefinition' {} a -> s {targetIds = a} :: ScpActionDefinition) Prelude.. Lens._Coerce

instance Core.FromJSON ScpActionDefinition where
  parseJSON =
    Core.withObject
      "ScpActionDefinition"
      ( \x ->
          ScpActionDefinition'
            Prelude.<$> (x Core..: "PolicyId")
            Prelude.<*> (x Core..: "TargetIds")
      )

instance Prelude.Hashable ScpActionDefinition

instance Prelude.NFData ScpActionDefinition

instance Core.ToJSON ScpActionDefinition where
  toJSON ScpActionDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Core..= policyId),
            Prelude.Just ("TargetIds" Core..= targetIds)
          ]
      )
