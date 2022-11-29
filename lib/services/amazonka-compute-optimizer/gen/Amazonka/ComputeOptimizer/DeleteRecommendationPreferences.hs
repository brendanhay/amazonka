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
-- Module      : Amazonka.ComputeOptimizer.DeleteRecommendationPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a recommendation preference, such as enhanced infrastructure
-- metrics.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
module Amazonka.ComputeOptimizer.DeleteRecommendationPreferences
  ( -- * Creating a Request
    DeleteRecommendationPreferences (..),
    newDeleteRecommendationPreferences,

    -- * Request Lenses
    deleteRecommendationPreferences_scope,
    deleteRecommendationPreferences_resourceType,
    deleteRecommendationPreferences_recommendationPreferenceNames,

    -- * Destructuring the Response
    DeleteRecommendationPreferencesResponse (..),
    newDeleteRecommendationPreferencesResponse,

    -- * Response Lenses
    deleteRecommendationPreferencesResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecommendationPreferences' smart constructor.
data DeleteRecommendationPreferences = DeleteRecommendationPreferences'
  { -- | An object that describes the scope of the recommendation preference to
    -- delete.
    --
    -- You can delete recommendation preferences that are created at the
    -- organization level (for management accounts of an organization only),
    -- account level, and resource level. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    scope :: Prelude.Maybe Scope,
    -- | The target resource type of the recommendation preference to delete.
    --
    -- The @Ec2Instance@ option encompasses standalone instances and instances
    -- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
    -- encompasses only instances that are part of an Auto Scaling group.
    --
    -- The valid values for this parameter are @Ec2Instance@ and
    -- @AutoScalingGroup@.
    resourceType :: ResourceType,
    -- | The name of the recommendation preference to delete.
    --
    -- Enhanced infrastructure metrics (@EnhancedInfrastructureMetrics@) is the
    -- only feature that can be activated through preferences. Therefore, it is
    -- also the only recommendation preference that can be deleted.
    recommendationPreferenceNames :: [RecommendationPreferenceName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommendationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'deleteRecommendationPreferences_scope' - An object that describes the scope of the recommendation preference to
-- delete.
--
-- You can delete recommendation preferences that are created at the
-- organization level (for management accounts of an organization only),
-- account level, and resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- 'resourceType', 'deleteRecommendationPreferences_resourceType' - The target resource type of the recommendation preference to delete.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
--
-- 'recommendationPreferenceNames', 'deleteRecommendationPreferences_recommendationPreferenceNames' - The name of the recommendation preference to delete.
--
-- Enhanced infrastructure metrics (@EnhancedInfrastructureMetrics@) is the
-- only feature that can be activated through preferences. Therefore, it is
-- also the only recommendation preference that can be deleted.
newDeleteRecommendationPreferences ::
  -- | 'resourceType'
  ResourceType ->
  DeleteRecommendationPreferences
newDeleteRecommendationPreferences pResourceType_ =
  DeleteRecommendationPreferences'
    { scope =
        Prelude.Nothing,
      resourceType = pResourceType_,
      recommendationPreferenceNames =
        Prelude.mempty
    }

-- | An object that describes the scope of the recommendation preference to
-- delete.
--
-- You can delete recommendation preferences that are created at the
-- organization level (for management accounts of an organization only),
-- account level, and resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
deleteRecommendationPreferences_scope :: Lens.Lens' DeleteRecommendationPreferences (Prelude.Maybe Scope)
deleteRecommendationPreferences_scope = Lens.lens (\DeleteRecommendationPreferences' {scope} -> scope) (\s@DeleteRecommendationPreferences' {} a -> s {scope = a} :: DeleteRecommendationPreferences)

-- | The target resource type of the recommendation preference to delete.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- The valid values for this parameter are @Ec2Instance@ and
-- @AutoScalingGroup@.
deleteRecommendationPreferences_resourceType :: Lens.Lens' DeleteRecommendationPreferences ResourceType
deleteRecommendationPreferences_resourceType = Lens.lens (\DeleteRecommendationPreferences' {resourceType} -> resourceType) (\s@DeleteRecommendationPreferences' {} a -> s {resourceType = a} :: DeleteRecommendationPreferences)

-- | The name of the recommendation preference to delete.
--
-- Enhanced infrastructure metrics (@EnhancedInfrastructureMetrics@) is the
-- only feature that can be activated through preferences. Therefore, it is
-- also the only recommendation preference that can be deleted.
deleteRecommendationPreferences_recommendationPreferenceNames :: Lens.Lens' DeleteRecommendationPreferences [RecommendationPreferenceName]
deleteRecommendationPreferences_recommendationPreferenceNames = Lens.lens (\DeleteRecommendationPreferences' {recommendationPreferenceNames} -> recommendationPreferenceNames) (\s@DeleteRecommendationPreferences' {} a -> s {recommendationPreferenceNames = a} :: DeleteRecommendationPreferences) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeleteRecommendationPreferences
  where
  type
    AWSResponse DeleteRecommendationPreferences =
      DeleteRecommendationPreferencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRecommendationPreferencesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteRecommendationPreferences
  where
  hashWithSalt
    _salt
    DeleteRecommendationPreferences' {..} =
      _salt `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` recommendationPreferenceNames

instance
  Prelude.NFData
    DeleteRecommendationPreferences
  where
  rnf DeleteRecommendationPreferences' {..} =
    Prelude.rnf scope
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf recommendationPreferenceNames

instance
  Core.ToHeaders
    DeleteRecommendationPreferences
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComputeOptimizerService.DeleteRecommendationPreferences" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRecommendationPreferences where
  toJSON DeleteRecommendationPreferences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scope" Core..=) Prelude.<$> scope,
            Prelude.Just ("resourceType" Core..= resourceType),
            Prelude.Just
              ( "recommendationPreferenceNames"
                  Core..= recommendationPreferenceNames
              )
          ]
      )

instance Core.ToPath DeleteRecommendationPreferences where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRecommendationPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecommendationPreferencesResponse' smart constructor.
data DeleteRecommendationPreferencesResponse = DeleteRecommendationPreferencesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecommendationPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRecommendationPreferencesResponse_httpStatus' - The response's http status code.
newDeleteRecommendationPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRecommendationPreferencesResponse
newDeleteRecommendationPreferencesResponse
  pHttpStatus_ =
    DeleteRecommendationPreferencesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteRecommendationPreferencesResponse_httpStatus :: Lens.Lens' DeleteRecommendationPreferencesResponse Prelude.Int
deleteRecommendationPreferencesResponse_httpStatus = Lens.lens (\DeleteRecommendationPreferencesResponse' {httpStatus} -> httpStatus) (\s@DeleteRecommendationPreferencesResponse' {} a -> s {httpStatus = a} :: DeleteRecommendationPreferencesResponse)

instance
  Prelude.NFData
    DeleteRecommendationPreferencesResponse
  where
  rnf DeleteRecommendationPreferencesResponse' {..} =
    Prelude.rnf httpStatus
