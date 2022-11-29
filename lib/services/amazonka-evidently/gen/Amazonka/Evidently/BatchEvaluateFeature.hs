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
-- Module      : Amazonka.Evidently.BatchEvaluateFeature
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation assigns feature variation to user sessions. For each user
-- session, you pass in an @entityID@ that represents the user. Evidently
-- then checks the evaluation rules and assigns the variation.
--
-- The first rules that are evaluated are the override rules. If the
-- user\'s @entityID@ matches an override rule, the user is served the
-- variation specified by that rule.
--
-- Next, if there is a launch of the feature, the user might be assigned to
-- a variation in the launch. The chance of this depends on the percentage
-- of users that are allocated to that launch. If the user is enrolled in
-- the launch, the variation they are served depends on the allocation of
-- the various feature variations used for the launch.
--
-- If the user is not assigned to a launch, and there is an ongoing
-- experiment for this feature, the user might be assigned to a variation
-- in the experiment. The chance of this depends on the percentage of users
-- that are allocated to that experiment. If the user is enrolled in the
-- experiment, the variation they are served depends on the allocation of
-- the various feature variations used for the experiment.
--
-- If the user is not assigned to a launch or experiment, they are served
-- the default variation.
module Amazonka.Evidently.BatchEvaluateFeature
  ( -- * Creating a Request
    BatchEvaluateFeature (..),
    newBatchEvaluateFeature,

    -- * Request Lenses
    batchEvaluateFeature_project,
    batchEvaluateFeature_requests,

    -- * Destructuring the Response
    BatchEvaluateFeatureResponse (..),
    newBatchEvaluateFeatureResponse,

    -- * Response Lenses
    batchEvaluateFeatureResponse_results,
    batchEvaluateFeatureResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchEvaluateFeature' smart constructor.
data BatchEvaluateFeature = BatchEvaluateFeature'
  { -- | The name or ARN of the project that contains the feature being
    -- evaluated.
    project :: Prelude.Text,
    -- | An array of structures, where each structure assigns a feature variation
    -- to one user session.
    requests :: Prelude.NonEmpty EvaluationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEvaluateFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'batchEvaluateFeature_project' - The name or ARN of the project that contains the feature being
-- evaluated.
--
-- 'requests', 'batchEvaluateFeature_requests' - An array of structures, where each structure assigns a feature variation
-- to one user session.
newBatchEvaluateFeature ::
  -- | 'project'
  Prelude.Text ->
  -- | 'requests'
  Prelude.NonEmpty EvaluationRequest ->
  BatchEvaluateFeature
newBatchEvaluateFeature pProject_ pRequests_ =
  BatchEvaluateFeature'
    { project = pProject_,
      requests = Lens.coerced Lens.# pRequests_
    }

-- | The name or ARN of the project that contains the feature being
-- evaluated.
batchEvaluateFeature_project :: Lens.Lens' BatchEvaluateFeature Prelude.Text
batchEvaluateFeature_project = Lens.lens (\BatchEvaluateFeature' {project} -> project) (\s@BatchEvaluateFeature' {} a -> s {project = a} :: BatchEvaluateFeature)

-- | An array of structures, where each structure assigns a feature variation
-- to one user session.
batchEvaluateFeature_requests :: Lens.Lens' BatchEvaluateFeature (Prelude.NonEmpty EvaluationRequest)
batchEvaluateFeature_requests = Lens.lens (\BatchEvaluateFeature' {requests} -> requests) (\s@BatchEvaluateFeature' {} a -> s {requests = a} :: BatchEvaluateFeature) Prelude.. Lens.coerced

instance Core.AWSRequest BatchEvaluateFeature where
  type
    AWSResponse BatchEvaluateFeature =
      BatchEvaluateFeatureResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchEvaluateFeatureResponse'
            Prelude.<$> (x Core..?> "results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchEvaluateFeature where
  hashWithSalt _salt BatchEvaluateFeature' {..} =
    _salt `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` requests

instance Prelude.NFData BatchEvaluateFeature where
  rnf BatchEvaluateFeature' {..} =
    Prelude.rnf project
      `Prelude.seq` Prelude.rnf requests

instance Core.ToHeaders BatchEvaluateFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchEvaluateFeature where
  toJSON BatchEvaluateFeature' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("requests" Core..= requests)]
      )

instance Core.ToPath BatchEvaluateFeature where
  toPath BatchEvaluateFeature' {..} =
    Prelude.mconcat
      ["/projects/", Core.toBS project, "/evaluations"]

instance Core.ToQuery BatchEvaluateFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchEvaluateFeatureResponse' smart constructor.
data BatchEvaluateFeatureResponse = BatchEvaluateFeatureResponse'
  { -- | An array of structures, where each structure displays the results of one
    -- feature evaluation assignment to one user session.
    results :: Prelude.Maybe [EvaluationResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEvaluateFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'batchEvaluateFeatureResponse_results' - An array of structures, where each structure displays the results of one
-- feature evaluation assignment to one user session.
--
-- 'httpStatus', 'batchEvaluateFeatureResponse_httpStatus' - The response's http status code.
newBatchEvaluateFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchEvaluateFeatureResponse
newBatchEvaluateFeatureResponse pHttpStatus_ =
  BatchEvaluateFeatureResponse'
    { results =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures, where each structure displays the results of one
-- feature evaluation assignment to one user session.
batchEvaluateFeatureResponse_results :: Lens.Lens' BatchEvaluateFeatureResponse (Prelude.Maybe [EvaluationResult])
batchEvaluateFeatureResponse_results = Lens.lens (\BatchEvaluateFeatureResponse' {results} -> results) (\s@BatchEvaluateFeatureResponse' {} a -> s {results = a} :: BatchEvaluateFeatureResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchEvaluateFeatureResponse_httpStatus :: Lens.Lens' BatchEvaluateFeatureResponse Prelude.Int
batchEvaluateFeatureResponse_httpStatus = Lens.lens (\BatchEvaluateFeatureResponse' {httpStatus} -> httpStatus) (\s@BatchEvaluateFeatureResponse' {} a -> s {httpStatus = a} :: BatchEvaluateFeatureResponse)

instance Prelude.NFData BatchEvaluateFeatureResponse where
  rnf BatchEvaluateFeatureResponse' {..} =
    Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
