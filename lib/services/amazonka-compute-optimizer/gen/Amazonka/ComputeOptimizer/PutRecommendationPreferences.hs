{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.PutRecommendationPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new recommendation preference or updates an existing
-- recommendation preference, such as enhanced infrastructure metrics.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.PutRecommendationPreferences
  ( -- * Creating a Request
    PutRecommendationPreferences (..),
    newPutRecommendationPreferences,

    -- * Request Lenses
    putRecommendationPreferences_enhancedInfrastructureMetrics,
    putRecommendationPreferences_externalMetricsPreference,
    putRecommendationPreferences_inferredWorkloadTypes,
    putRecommendationPreferences_scope,
    putRecommendationPreferences_resourceType,

    -- * Destructuring the Response
    PutRecommendationPreferencesResponse (..),
    newPutRecommendationPreferencesResponse,

    -- * Response Lenses
    putRecommendationPreferencesResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRecommendationPreferences' smart constructor.
data PutRecommendationPreferences = PutRecommendationPreferences'
  { -- | The status of the enhanced infrastructure metrics recommendation
    -- preference to create or update.
    --
    -- Specify the @Active@ status to activate the preference, or specify
    -- @Inactive@ to deactivate the preference.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    enhancedInfrastructureMetrics :: Prelude.Maybe EnhancedInfrastructureMetrics,
    -- | The provider of the external metrics recommendation preference to create
    -- or update.
    --
    -- Specify a valid provider in the @source@ field to activate the
    -- preference. To delete this preference, see the
    -- DeleteRecommendationPreferences action.
    --
    -- This preference can only be set for the @Ec2Instance@ resource type.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/external-metrics-ingestion.html External metrics ingestion>
    -- in the /Compute Optimizer User Guide/.
    externalMetricsPreference :: Prelude.Maybe ExternalMetricsPreference,
    -- | The status of the inferred workload types recommendation preference to
    -- create or update.
    --
    -- The inferred workload type feature is active by default. To deactivate
    -- it, create a recommendation preference.
    --
    -- Specify the @Inactive@ status to deactivate the feature, or specify
    -- @Active@ to activate it.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/inferred-workload-types.html Inferred workload types>
    -- in the /Compute Optimizer User Guide/.
    inferredWorkloadTypes :: Prelude.Maybe InferredWorkloadTypesPreference,
    -- | An object that describes the scope of the recommendation preference to
    -- create.
    --
    -- You can create recommendation preferences at the organization level (for
    -- management accounts of an organization only), account level, and
    -- resource level. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    --
    -- You cannot create recommendation preferences for Auto Scaling groups at
    -- the organization and account levels. You can create recommendation
    -- preferences for Auto Scaling groups only at the resource level by
    -- specifying a scope name of @ResourceArn@ and a scope value of the Auto
    -- Scaling group Amazon Resource Name (ARN). This will configure the
    -- preference for all instances that are part of the specified Auto Scaling
    -- group. You also cannot create recommendation preferences at the resource
    -- level for instances that are part of an Auto Scaling group. You can
    -- create recommendation preferences at the resource level only for
    -- standalone instances.
    scope :: Prelude.Maybe Scope,
    -- | The target resource type of the recommendation preference to create.
    --
    -- The @Ec2Instance@ option encompasses standalone instances and instances
    -- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
    -- encompasses only instances that are part of an Auto Scaling group.
    --
    -- The valid values for this parameter are @Ec2Instance@ and
    -- @AutoScalingGroup@.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecommendationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enhancedInfrastructureMetrics', 'putRecommendationPreferences_enhancedInfrastructureMetrics' - The status of the enhanced infrastructure metrics recommendation
-- preference to create or update.
--
-- Specify the @Active@ status to activate the preference, or specify
-- @Inactive@ to deactivate the preference.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- 'externalMetricsPreference', 'putRecommendationPreferences_externalMetricsPreference' - The provider of the external metrics recommendation preference to create
-- or update.
--
-- Specify a valid provider in the @source@ field to activate the
-- preference. To delete this preference, see the
-- DeleteRecommendationPreferences action.
--
-- This preference can only be set for the @Ec2Instance@ resource type.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/external-metrics-ingestion.html External metrics ingestion>
-- in the /Compute Optimizer User Guide/.
--
-- 'inferredWorkloadTypes', 'putRecommendationPreferences_inferredWorkloadTypes' - The status of the inferred workload types recommendation preference to
-- create or update.
--
-- The inferred workload type feature is active by default. To deactivate
-- it, create a recommendation preference.
--
-- Specify the @Inactive@ status to deactivate the feature, or specify
-- @Active@ to activate it.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/inferred-workload-types.html Inferred workload types>
-- in the /Compute Optimizer User Guide/.
--
-- 'scope', 'putRecommendationPreferences_scope' - An object that describes the scope of the recommendation preference to
-- create.
--
-- You can create recommendation preferences at the organization level (for
-- management accounts of an organization only), account level, and
-- resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- You cannot create recommendation preferences for Auto Scaling groups at
-- the organization and account levels. You can create recommendation
-- preferences for Auto Scaling groups only at the resource level by
-- specifying a scope name of @ResourceArn@ and a scope value of the Auto
-- Scaling group Amazon Resource Name (ARN). This will configure the
-- preference for all instances that are part of the specified Auto Scaling
-- group. You also cannot create recommendation preferences at the resource
-- level for instances that are part of an Auto Scaling group. You can
-- create recommendation preferences at the resource level only for
-- standalone instances.
--
-- 'resourceType', 'putRecommendationPreferences_resourceType' - The target resource type of the recommendation preference to create.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
newPutRecommendationPreferences ::
  -- | 'resourceType'
  ResourceType ->
  PutRecommendationPreferences
newPutRecommendationPreferences pResourceType_ =
  PutRecommendationPreferences'
    { enhancedInfrastructureMetrics =
        Prelude.Nothing,
      externalMetricsPreference = Prelude.Nothing,
      inferredWorkloadTypes = Prelude.Nothing,
      scope = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | The status of the enhanced infrastructure metrics recommendation
-- preference to create or update.
--
-- Specify the @Active@ status to activate the preference, or specify
-- @Inactive@ to deactivate the preference.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
putRecommendationPreferences_enhancedInfrastructureMetrics :: Lens.Lens' PutRecommendationPreferences (Prelude.Maybe EnhancedInfrastructureMetrics)
putRecommendationPreferences_enhancedInfrastructureMetrics = Lens.lens (\PutRecommendationPreferences' {enhancedInfrastructureMetrics} -> enhancedInfrastructureMetrics) (\s@PutRecommendationPreferences' {} a -> s {enhancedInfrastructureMetrics = a} :: PutRecommendationPreferences)

-- | The provider of the external metrics recommendation preference to create
-- or update.
--
-- Specify a valid provider in the @source@ field to activate the
-- preference. To delete this preference, see the
-- DeleteRecommendationPreferences action.
--
-- This preference can only be set for the @Ec2Instance@ resource type.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/external-metrics-ingestion.html External metrics ingestion>
-- in the /Compute Optimizer User Guide/.
putRecommendationPreferences_externalMetricsPreference :: Lens.Lens' PutRecommendationPreferences (Prelude.Maybe ExternalMetricsPreference)
putRecommendationPreferences_externalMetricsPreference = Lens.lens (\PutRecommendationPreferences' {externalMetricsPreference} -> externalMetricsPreference) (\s@PutRecommendationPreferences' {} a -> s {externalMetricsPreference = a} :: PutRecommendationPreferences)

-- | The status of the inferred workload types recommendation preference to
-- create or update.
--
-- The inferred workload type feature is active by default. To deactivate
-- it, create a recommendation preference.
--
-- Specify the @Inactive@ status to deactivate the feature, or specify
-- @Active@ to activate it.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/inferred-workload-types.html Inferred workload types>
-- in the /Compute Optimizer User Guide/.
putRecommendationPreferences_inferredWorkloadTypes :: Lens.Lens' PutRecommendationPreferences (Prelude.Maybe InferredWorkloadTypesPreference)
putRecommendationPreferences_inferredWorkloadTypes = Lens.lens (\PutRecommendationPreferences' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@PutRecommendationPreferences' {} a -> s {inferredWorkloadTypes = a} :: PutRecommendationPreferences)

-- | An object that describes the scope of the recommendation preference to
-- create.
--
-- You can create recommendation preferences at the organization level (for
-- management accounts of an organization only), account level, and
-- resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- You cannot create recommendation preferences for Auto Scaling groups at
-- the organization and account levels. You can create recommendation
-- preferences for Auto Scaling groups only at the resource level by
-- specifying a scope name of @ResourceArn@ and a scope value of the Auto
-- Scaling group Amazon Resource Name (ARN). This will configure the
-- preference for all instances that are part of the specified Auto Scaling
-- group. You also cannot create recommendation preferences at the resource
-- level for instances that are part of an Auto Scaling group. You can
-- create recommendation preferences at the resource level only for
-- standalone instances.
putRecommendationPreferences_scope :: Lens.Lens' PutRecommendationPreferences (Prelude.Maybe Scope)
putRecommendationPreferences_scope = Lens.lens (\PutRecommendationPreferences' {scope} -> scope) (\s@PutRecommendationPreferences' {} a -> s {scope = a} :: PutRecommendationPreferences)

-- | The target resource type of the recommendation preference to create.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
putRecommendationPreferences_resourceType :: Lens.Lens' PutRecommendationPreferences ResourceType
putRecommendationPreferences_resourceType = Lens.lens (\PutRecommendationPreferences' {resourceType} -> resourceType) (\s@PutRecommendationPreferences' {} a -> s {resourceType = a} :: PutRecommendationPreferences)

instance Core.AWSRequest PutRecommendationPreferences where
  type
    AWSResponse PutRecommendationPreferences =
      PutRecommendationPreferencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRecommendationPreferencesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutRecommendationPreferences
  where
  hashWithSalt _salt PutRecommendationPreferences' {..} =
    _salt
      `Prelude.hashWithSalt` enhancedInfrastructureMetrics
      `Prelude.hashWithSalt` externalMetricsPreference
      `Prelude.hashWithSalt` inferredWorkloadTypes
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData PutRecommendationPreferences where
  rnf PutRecommendationPreferences' {..} =
    Prelude.rnf enhancedInfrastructureMetrics
      `Prelude.seq` Prelude.rnf externalMetricsPreference
      `Prelude.seq` Prelude.rnf inferredWorkloadTypes
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders PutRecommendationPreferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.PutRecommendationPreferences" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRecommendationPreferences where
  toJSON PutRecommendationPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enhancedInfrastructureMetrics" Data..=)
              Prelude.<$> enhancedInfrastructureMetrics,
            ("externalMetricsPreference" Data..=)
              Prelude.<$> externalMetricsPreference,
            ("inferredWorkloadTypes" Data..=)
              Prelude.<$> inferredWorkloadTypes,
            ("scope" Data..=) Prelude.<$> scope,
            Prelude.Just ("resourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath PutRecommendationPreferences where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRecommendationPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRecommendationPreferencesResponse' smart constructor.
data PutRecommendationPreferencesResponse = PutRecommendationPreferencesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRecommendationPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRecommendationPreferencesResponse_httpStatus' - The response's http status code.
newPutRecommendationPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRecommendationPreferencesResponse
newPutRecommendationPreferencesResponse pHttpStatus_ =
  PutRecommendationPreferencesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRecommendationPreferencesResponse_httpStatus :: Lens.Lens' PutRecommendationPreferencesResponse Prelude.Int
putRecommendationPreferencesResponse_httpStatus = Lens.lens (\PutRecommendationPreferencesResponse' {httpStatus} -> httpStatus) (\s@PutRecommendationPreferencesResponse' {} a -> s {httpStatus = a} :: PutRecommendationPreferencesResponse)

instance
  Prelude.NFData
    PutRecommendationPreferencesResponse
  where
  rnf PutRecommendationPreferencesResponse' {..} =
    Prelude.rnf httpStatus
