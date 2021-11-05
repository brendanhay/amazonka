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
-- Module      : Network.AWS.MacieV2.Types.PolicyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.PolicyDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.FindingAction
import Network.AWS.MacieV2.Types.FindingActor
import qualified Network.AWS.Prelude as Prelude

-- | Provides the details of a policy finding.
--
-- /See:/ 'newPolicyDetails' smart constructor.
data PolicyDetails = PolicyDetails'
  { -- | The entity that performed the action that produced the finding.
    actor :: Prelude.Maybe FindingActor,
    -- | The action that produced the finding.
    action :: Prelude.Maybe FindingAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actor', 'policyDetails_actor' - The entity that performed the action that produced the finding.
--
-- 'action', 'policyDetails_action' - The action that produced the finding.
newPolicyDetails ::
  PolicyDetails
newPolicyDetails =
  PolicyDetails'
    { actor = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The entity that performed the action that produced the finding.
policyDetails_actor :: Lens.Lens' PolicyDetails (Prelude.Maybe FindingActor)
policyDetails_actor = Lens.lens (\PolicyDetails' {actor} -> actor) (\s@PolicyDetails' {} a -> s {actor = a} :: PolicyDetails)

-- | The action that produced the finding.
policyDetails_action :: Lens.Lens' PolicyDetails (Prelude.Maybe FindingAction)
policyDetails_action = Lens.lens (\PolicyDetails' {action} -> action) (\s@PolicyDetails' {} a -> s {action = a} :: PolicyDetails)

instance Core.FromJSON PolicyDetails where
  parseJSON =
    Core.withObject
      "PolicyDetails"
      ( \x ->
          PolicyDetails'
            Prelude.<$> (x Core..:? "actor")
            Prelude.<*> (x Core..:? "action")
      )

instance Prelude.Hashable PolicyDetails

instance Prelude.NFData PolicyDetails
