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
-- Module      : Amazonka.EKS.Types.OutpostConfigRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.OutpostConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types.ControlPlanePlacementRequest
import qualified Amazonka.Prelude as Prelude

-- | The configuration of your local Amazon EKS cluster on an Amazon Web
-- Services Outpost. Before creating a cluster on an Outpost, review
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-local-cluster-create.html Creating a local cluster on an Outpost>
-- in the /Amazon EKS User Guide/. This API isn\'t available for Amazon EKS
-- clusters on the Amazon Web Services cloud.
--
-- /See:/ 'newOutpostConfigRequest' smart constructor.
data OutpostConfigRequest = OutpostConfigRequest'
  { -- | An object representing the placement configuration for all the control
    -- plane instance of your local Amazon EKS cluster on an Amazon Web
    -- Services Outpost. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
    -- in the /Amazon EKS User Guide/.
    controlPlanePlacement :: Prelude.Maybe ControlPlanePlacementRequest,
    -- | The ARN of the Outpost that you want to use for your local Amazon EKS
    -- cluster on Outposts. Only a single Outpost ARN is supported.
    outpostArns :: [Prelude.Text],
    -- | The Amazon EC2 instance type that you want to use for your local Amazon
    -- EKS cluster on Outposts. Choose an instance type based on the number of
    -- nodes that your cluster will have. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
    -- in the /Amazon EKS User Guide/.
    --
    -- The instance type that you specify is used for all Kubernetes control
    -- plane instances. The instance type can\'t be changed after cluster
    -- creation. The control plane is not automatically scaled by Amazon EKS.
    controlPlaneInstanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutpostConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPlanePlacement', 'outpostConfigRequest_controlPlanePlacement' - An object representing the placement configuration for all the control
-- plane instance of your local Amazon EKS cluster on an Amazon Web
-- Services Outpost. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/.
--
-- 'outpostArns', 'outpostConfigRequest_outpostArns' - The ARN of the Outpost that you want to use for your local Amazon EKS
-- cluster on Outposts. Only a single Outpost ARN is supported.
--
-- 'controlPlaneInstanceType', 'outpostConfigRequest_controlPlaneInstanceType' - The Amazon EC2 instance type that you want to use for your local Amazon
-- EKS cluster on Outposts. Choose an instance type based on the number of
-- nodes that your cluster will have. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/.
--
-- The instance type that you specify is used for all Kubernetes control
-- plane instances. The instance type can\'t be changed after cluster
-- creation. The control plane is not automatically scaled by Amazon EKS.
newOutpostConfigRequest ::
  -- | 'controlPlaneInstanceType'
  Prelude.Text ->
  OutpostConfigRequest
newOutpostConfigRequest pControlPlaneInstanceType_ =
  OutpostConfigRequest'
    { controlPlanePlacement =
        Prelude.Nothing,
      outpostArns = Prelude.mempty,
      controlPlaneInstanceType =
        pControlPlaneInstanceType_
    }

-- | An object representing the placement configuration for all the control
-- plane instance of your local Amazon EKS cluster on an Amazon Web
-- Services Outpost. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/.
outpostConfigRequest_controlPlanePlacement :: Lens.Lens' OutpostConfigRequest (Prelude.Maybe ControlPlanePlacementRequest)
outpostConfigRequest_controlPlanePlacement = Lens.lens (\OutpostConfigRequest' {controlPlanePlacement} -> controlPlanePlacement) (\s@OutpostConfigRequest' {} a -> s {controlPlanePlacement = a} :: OutpostConfigRequest)

-- | The ARN of the Outpost that you want to use for your local Amazon EKS
-- cluster on Outposts. Only a single Outpost ARN is supported.
outpostConfigRequest_outpostArns :: Lens.Lens' OutpostConfigRequest [Prelude.Text]
outpostConfigRequest_outpostArns = Lens.lens (\OutpostConfigRequest' {outpostArns} -> outpostArns) (\s@OutpostConfigRequest' {} a -> s {outpostArns = a} :: OutpostConfigRequest) Prelude.. Lens.coerced

-- | The Amazon EC2 instance type that you want to use for your local Amazon
-- EKS cluster on Outposts. Choose an instance type based on the number of
-- nodes that your cluster will have. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-outposts-capacity-considerations.html Capacity considerations>
-- in the /Amazon EKS User Guide/.
--
-- The instance type that you specify is used for all Kubernetes control
-- plane instances. The instance type can\'t be changed after cluster
-- creation. The control plane is not automatically scaled by Amazon EKS.
outpostConfigRequest_controlPlaneInstanceType :: Lens.Lens' OutpostConfigRequest Prelude.Text
outpostConfigRequest_controlPlaneInstanceType = Lens.lens (\OutpostConfigRequest' {controlPlaneInstanceType} -> controlPlaneInstanceType) (\s@OutpostConfigRequest' {} a -> s {controlPlaneInstanceType = a} :: OutpostConfigRequest)

instance Prelude.Hashable OutpostConfigRequest where
  hashWithSalt _salt OutpostConfigRequest' {..} =
    _salt `Prelude.hashWithSalt` controlPlanePlacement
      `Prelude.hashWithSalt` outpostArns
      `Prelude.hashWithSalt` controlPlaneInstanceType

instance Prelude.NFData OutpostConfigRequest where
  rnf OutpostConfigRequest' {..} =
    Prelude.rnf controlPlanePlacement
      `Prelude.seq` Prelude.rnf outpostArns
      `Prelude.seq` Prelude.rnf controlPlaneInstanceType

instance Core.ToJSON OutpostConfigRequest where
  toJSON OutpostConfigRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("controlPlanePlacement" Core..=)
              Prelude.<$> controlPlanePlacement,
            Prelude.Just ("outpostArns" Core..= outpostArns),
            Prelude.Just
              ( "controlPlaneInstanceType"
                  Core..= controlPlaneInstanceType
              )
          ]
      )
