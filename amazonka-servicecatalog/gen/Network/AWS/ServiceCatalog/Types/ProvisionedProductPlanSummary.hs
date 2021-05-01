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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType

-- | Summary information about a plan.
--
-- /See:/ 'newProvisionedProductPlanSummary' smart constructor.
data ProvisionedProductPlanSummary = ProvisionedProductPlanSummary'
  { -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The name of the plan.
    planName :: Prelude.Maybe Prelude.Text,
    -- | The plan identifier.
    planId :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    planType :: Prelude.Maybe ProvisionedProductPlanType,
    -- | The user-friendly name of the provisioned product.
    provisionProductName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedProductPlanSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionProductId', 'provisionedProductPlanSummary_provisionProductId' - The product identifier.
--
-- 'provisioningArtifactId', 'provisionedProductPlanSummary_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'planName', 'provisionedProductPlanSummary_planName' - The name of the plan.
--
-- 'planId', 'provisionedProductPlanSummary_planId' - The plan identifier.
--
-- 'planType', 'provisionedProductPlanSummary_planType' - The plan type.
--
-- 'provisionProductName', 'provisionedProductPlanSummary_provisionProductName' - The user-friendly name of the provisioned product.
newProvisionedProductPlanSummary ::
  ProvisionedProductPlanSummary
newProvisionedProductPlanSummary =
  ProvisionedProductPlanSummary'
    { provisionProductId =
        Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      planName = Prelude.Nothing,
      planId = Prelude.Nothing,
      planType = Prelude.Nothing,
      provisionProductName = Prelude.Nothing
    }

-- | The product identifier.
provisionedProductPlanSummary_provisionProductId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisionProductId = Lens.lens (\ProvisionedProductPlanSummary' {provisionProductId} -> provisionProductId) (\s@ProvisionedProductPlanSummary' {} a -> s {provisionProductId = a} :: ProvisionedProductPlanSummary)

-- | The identifier of the provisioning artifact.
provisionedProductPlanSummary_provisioningArtifactId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisioningArtifactId = Lens.lens (\ProvisionedProductPlanSummary' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionedProductPlanSummary' {} a -> s {provisioningArtifactId = a} :: ProvisionedProductPlanSummary)

-- | The name of the plan.
provisionedProductPlanSummary_planName :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_planName = Lens.lens (\ProvisionedProductPlanSummary' {planName} -> planName) (\s@ProvisionedProductPlanSummary' {} a -> s {planName = a} :: ProvisionedProductPlanSummary)

-- | The plan identifier.
provisionedProductPlanSummary_planId :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_planId = Lens.lens (\ProvisionedProductPlanSummary' {planId} -> planId) (\s@ProvisionedProductPlanSummary' {} a -> s {planId = a} :: ProvisionedProductPlanSummary)

-- | The plan type.
provisionedProductPlanSummary_planType :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe ProvisionedProductPlanType)
provisionedProductPlanSummary_planType = Lens.lens (\ProvisionedProductPlanSummary' {planType} -> planType) (\s@ProvisionedProductPlanSummary' {} a -> s {planType = a} :: ProvisionedProductPlanSummary)

-- | The user-friendly name of the provisioned product.
provisionedProductPlanSummary_provisionProductName :: Lens.Lens' ProvisionedProductPlanSummary (Prelude.Maybe Prelude.Text)
provisionedProductPlanSummary_provisionProductName = Lens.lens (\ProvisionedProductPlanSummary' {provisionProductName} -> provisionProductName) (\s@ProvisionedProductPlanSummary' {} a -> s {provisionProductName = a} :: ProvisionedProductPlanSummary)

instance
  Prelude.FromJSON
    ProvisionedProductPlanSummary
  where
  parseJSON =
    Prelude.withObject
      "ProvisionedProductPlanSummary"
      ( \x ->
          ProvisionedProductPlanSummary'
            Prelude.<$> (x Prelude..:? "ProvisionProductId")
            Prelude.<*> (x Prelude..:? "ProvisioningArtifactId")
            Prelude.<*> (x Prelude..:? "PlanName")
            Prelude.<*> (x Prelude..:? "PlanId")
            Prelude.<*> (x Prelude..:? "PlanType")
            Prelude.<*> (x Prelude..:? "ProvisionProductName")
      )

instance
  Prelude.Hashable
    ProvisionedProductPlanSummary

instance Prelude.NFData ProvisionedProductPlanSummary
