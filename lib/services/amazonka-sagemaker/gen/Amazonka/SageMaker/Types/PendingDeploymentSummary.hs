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
-- Module      : Amazonka.SageMaker.Types.PendingDeploymentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PendingDeploymentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.PendingProductionVariantSummary

-- | The summary of an in-progress deployment when an endpoint is creating or
-- updating with a new endpoint configuration.
--
-- /See:/ 'newPendingDeploymentSummary' smart constructor.
data PendingDeploymentSummary = PendingDeploymentSummary'
  { -- | An array of PendingProductionVariantSummary objects, one for each model
    -- hosted behind this endpoint for the in-progress deployment.
    productionVariants :: Prelude.Maybe (Prelude.NonEmpty PendingProductionVariantSummary),
    -- | An array of PendingProductionVariantSummary objects, one for each model
    -- hosted behind this endpoint in shadow mode with production traffic
    -- replicated from the model specified on @ProductionVariants@ for the
    -- in-progress deployment.
    shadowProductionVariants :: Prelude.Maybe (Prelude.NonEmpty PendingProductionVariantSummary),
    -- | The start time of the deployment.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the endpoint configuration used in the deployment.
    endpointConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingDeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productionVariants', 'pendingDeploymentSummary_productionVariants' - An array of PendingProductionVariantSummary objects, one for each model
-- hosted behind this endpoint for the in-progress deployment.
--
-- 'shadowProductionVariants', 'pendingDeploymentSummary_shadowProductionVariants' - An array of PendingProductionVariantSummary objects, one for each model
-- hosted behind this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@ for the
-- in-progress deployment.
--
-- 'startTime', 'pendingDeploymentSummary_startTime' - The start time of the deployment.
--
-- 'endpointConfigName', 'pendingDeploymentSummary_endpointConfigName' - The name of the endpoint configuration used in the deployment.
newPendingDeploymentSummary ::
  -- | 'endpointConfigName'
  Prelude.Text ->
  PendingDeploymentSummary
newPendingDeploymentSummary pEndpointConfigName_ =
  PendingDeploymentSummary'
    { productionVariants =
        Prelude.Nothing,
      shadowProductionVariants = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endpointConfigName = pEndpointConfigName_
    }

-- | An array of PendingProductionVariantSummary objects, one for each model
-- hosted behind this endpoint for the in-progress deployment.
pendingDeploymentSummary_productionVariants :: Lens.Lens' PendingDeploymentSummary (Prelude.Maybe (Prelude.NonEmpty PendingProductionVariantSummary))
pendingDeploymentSummary_productionVariants = Lens.lens (\PendingDeploymentSummary' {productionVariants} -> productionVariants) (\s@PendingDeploymentSummary' {} a -> s {productionVariants = a} :: PendingDeploymentSummary) Prelude.. Lens.mapping Lens.coerced

-- | An array of PendingProductionVariantSummary objects, one for each model
-- hosted behind this endpoint in shadow mode with production traffic
-- replicated from the model specified on @ProductionVariants@ for the
-- in-progress deployment.
pendingDeploymentSummary_shadowProductionVariants :: Lens.Lens' PendingDeploymentSummary (Prelude.Maybe (Prelude.NonEmpty PendingProductionVariantSummary))
pendingDeploymentSummary_shadowProductionVariants = Lens.lens (\PendingDeploymentSummary' {shadowProductionVariants} -> shadowProductionVariants) (\s@PendingDeploymentSummary' {} a -> s {shadowProductionVariants = a} :: PendingDeploymentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The start time of the deployment.
pendingDeploymentSummary_startTime :: Lens.Lens' PendingDeploymentSummary (Prelude.Maybe Prelude.UTCTime)
pendingDeploymentSummary_startTime = Lens.lens (\PendingDeploymentSummary' {startTime} -> startTime) (\s@PendingDeploymentSummary' {} a -> s {startTime = a} :: PendingDeploymentSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the endpoint configuration used in the deployment.
pendingDeploymentSummary_endpointConfigName :: Lens.Lens' PendingDeploymentSummary Prelude.Text
pendingDeploymentSummary_endpointConfigName = Lens.lens (\PendingDeploymentSummary' {endpointConfigName} -> endpointConfigName) (\s@PendingDeploymentSummary' {} a -> s {endpointConfigName = a} :: PendingDeploymentSummary)

instance Data.FromJSON PendingDeploymentSummary where
  parseJSON =
    Data.withObject
      "PendingDeploymentSummary"
      ( \x ->
          PendingDeploymentSummary'
            Prelude.<$> (x Data..:? "ProductionVariants")
            Prelude.<*> (x Data..:? "ShadowProductionVariants")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..: "EndpointConfigName")
      )

instance Prelude.Hashable PendingDeploymentSummary where
  hashWithSalt _salt PendingDeploymentSummary' {..} =
    _salt `Prelude.hashWithSalt` productionVariants
      `Prelude.hashWithSalt` shadowProductionVariants
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endpointConfigName

instance Prelude.NFData PendingDeploymentSummary where
  rnf PendingDeploymentSummary' {..} =
    Prelude.rnf productionVariants
      `Prelude.seq` Prelude.rnf shadowProductionVariants
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endpointConfigName
