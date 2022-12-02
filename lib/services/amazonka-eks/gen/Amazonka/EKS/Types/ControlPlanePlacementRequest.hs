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
-- Module      : Amazonka.EKS.Types.ControlPlanePlacementRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ControlPlanePlacementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The placement configuration for all the control plane instance of your
-- local Amazon EKS cluster on an Amazon Web Services Outpost. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/
--
-- /See:/ 'newControlPlanePlacementRequest' smart constructor.
data ControlPlanePlacementRequest = ControlPlanePlacementRequest'
  { -- | The name of the placement group for the Kubernetes control plane
    -- instances. This setting can\'t be changed after cluster creation.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlPlanePlacementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'controlPlanePlacementRequest_groupName' - The name of the placement group for the Kubernetes control plane
-- instances. This setting can\'t be changed after cluster creation.
newControlPlanePlacementRequest ::
  ControlPlanePlacementRequest
newControlPlanePlacementRequest =
  ControlPlanePlacementRequest'
    { groupName =
        Prelude.Nothing
    }

-- | The name of the placement group for the Kubernetes control plane
-- instances. This setting can\'t be changed after cluster creation.
controlPlanePlacementRequest_groupName :: Lens.Lens' ControlPlanePlacementRequest (Prelude.Maybe Prelude.Text)
controlPlanePlacementRequest_groupName = Lens.lens (\ControlPlanePlacementRequest' {groupName} -> groupName) (\s@ControlPlanePlacementRequest' {} a -> s {groupName = a} :: ControlPlanePlacementRequest)

instance
  Prelude.Hashable
    ControlPlanePlacementRequest
  where
  hashWithSalt _salt ControlPlanePlacementRequest' {..} =
    _salt `Prelude.hashWithSalt` groupName

instance Prelude.NFData ControlPlanePlacementRequest where
  rnf ControlPlanePlacementRequest' {..} =
    Prelude.rnf groupName

instance Data.ToJSON ControlPlanePlacementRequest where
  toJSON ControlPlanePlacementRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("groupName" Data..=) Prelude.<$> groupName]
      )
