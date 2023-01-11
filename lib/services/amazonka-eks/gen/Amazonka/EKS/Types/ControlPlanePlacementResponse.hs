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
-- Module      : Amazonka.EKS.Types.ControlPlanePlacementResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ControlPlanePlacementResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The placement configuration for all the control plane instances of your
-- local Amazon EKS cluster on an Amazon Web Services Outpost. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/.
--
-- /See:/ 'newControlPlanePlacementResponse' smart constructor.
data ControlPlanePlacementResponse = ControlPlanePlacementResponse'
  { -- | The name of the placement group for the Kubernetes control plane
    -- instances.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlPlanePlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'controlPlanePlacementResponse_groupName' - The name of the placement group for the Kubernetes control plane
-- instances.
newControlPlanePlacementResponse ::
  ControlPlanePlacementResponse
newControlPlanePlacementResponse =
  ControlPlanePlacementResponse'
    { groupName =
        Prelude.Nothing
    }

-- | The name of the placement group for the Kubernetes control plane
-- instances.
controlPlanePlacementResponse_groupName :: Lens.Lens' ControlPlanePlacementResponse (Prelude.Maybe Prelude.Text)
controlPlanePlacementResponse_groupName = Lens.lens (\ControlPlanePlacementResponse' {groupName} -> groupName) (\s@ControlPlanePlacementResponse' {} a -> s {groupName = a} :: ControlPlanePlacementResponse)

instance Data.FromJSON ControlPlanePlacementResponse where
  parseJSON =
    Data.withObject
      "ControlPlanePlacementResponse"
      ( \x ->
          ControlPlanePlacementResponse'
            Prelude.<$> (x Data..:? "groupName")
      )

instance
  Prelude.Hashable
    ControlPlanePlacementResponse
  where
  hashWithSalt _salt ControlPlanePlacementResponse' {..} =
    _salt `Prelude.hashWithSalt` groupName

instance Prelude.NFData ControlPlanePlacementResponse where
  rnf ControlPlanePlacementResponse' {..} =
    Prelude.rnf groupName
