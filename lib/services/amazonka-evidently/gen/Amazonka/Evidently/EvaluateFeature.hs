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
-- Module      : Amazonka.Evidently.EvaluateFeature
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation assigns a feature variation to one given user session.
-- You pass in an @entityID@ that represents the user. Evidently then
-- checks the evaluation rules and assigns the variation.
--
-- The first rules that are evaluated are the override rules. If the
-- user\'s @entityID@ matches an override rule, the user is served the
-- variation specified by that rule.
--
-- >  <p>If there is a current launch with this feature that uses segment overrides, and if the user session's <code>evaluationContext</code> matches a segment rule defined in a segment override, the configuration in the segment overrides is used. For more information about segments, see <a href="https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateSegment.html">CreateSegment</a> and <a href="https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html">Use segments to focus your audience</a>.</p> <p>If there is a launch with no segment overrides, the user might be assigned to a variation in the launch. The chance of this depends on the percentage of users that are allocated to that launch. If the user is enrolled in the launch, the variation they are served depends on the allocation of the various feature variations used for the launch.</p> <p>If the user is not assigned to a launch, and there is an ongoing experiment for this feature, the user might be assigned to a variation in the experiment. The chance of this depends on the percentage of users that are allocated to that experiment.</p> <p>If the experiment uses a segment, then only user sessions with <code>evaluationContext</code> values that match the segment rule are used in the experiment.</p> <p>If the user is enrolled in the experiment, the variation they are served depends on the allocation of the various feature variations used for the experiment. </p> <p>If the user is not assigned to a launch or experiment, they are served the default variation.</p>
module Amazonka.Evidently.EvaluateFeature
  ( -- * Creating a Request
    EvaluateFeature (..),
    newEvaluateFeature,

    -- * Request Lenses
    evaluateFeature_evaluationContext,
    evaluateFeature_entityId,
    evaluateFeature_feature,
    evaluateFeature_project,

    -- * Destructuring the Response
    EvaluateFeatureResponse (..),
    newEvaluateFeatureResponse,

    -- * Response Lenses
    evaluateFeatureResponse_variation,
    evaluateFeatureResponse_details,
    evaluateFeatureResponse_reason,
    evaluateFeatureResponse_value,
    evaluateFeatureResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEvaluateFeature' smart constructor.
data EvaluateFeature = EvaluateFeature'
  { -- | A JSON object of attributes that you can optionally pass in as part of
    -- the evaluation event sent to Evidently from the user session. Evidently
    -- can use this value to match user sessions with defined audience
    -- segments. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Use segments to focus your audience>.
    --
    -- >  <p>If you include this parameter, the value must be a JSON object. A JSON array is not supported.</p>
    evaluationContext :: Prelude.Maybe Prelude.Text,
    -- | An internal ID that represents a unique user of the application. This
    -- @entityID@ is checked against any override rules assigned for this
    -- feature.
    entityId :: Prelude.Text,
    -- | The name of the feature being evaluated.
    feature :: Prelude.Text,
    -- | The name or ARN of the project that contains this feature.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationContext', 'evaluateFeature_evaluationContext' - A JSON object of attributes that you can optionally pass in as part of
-- the evaluation event sent to Evidently from the user session. Evidently
-- can use this value to match user sessions with defined audience
-- segments. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Use segments to focus your audience>.
--
-- >  <p>If you include this parameter, the value must be a JSON object. A JSON array is not supported.</p>
--
-- 'entityId', 'evaluateFeature_entityId' - An internal ID that represents a unique user of the application. This
-- @entityID@ is checked against any override rules assigned for this
-- feature.
--
-- 'feature', 'evaluateFeature_feature' - The name of the feature being evaluated.
--
-- 'project', 'evaluateFeature_project' - The name or ARN of the project that contains this feature.
newEvaluateFeature ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'feature'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  EvaluateFeature
newEvaluateFeature pEntityId_ pFeature_ pProject_ =
  EvaluateFeature'
    { evaluationContext =
        Prelude.Nothing,
      entityId = pEntityId_,
      feature = pFeature_,
      project = pProject_
    }

-- | A JSON object of attributes that you can optionally pass in as part of
-- the evaluation event sent to Evidently from the user session. Evidently
-- can use this value to match user sessions with defined audience
-- segments. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html Use segments to focus your audience>.
--
-- >  <p>If you include this parameter, the value must be a JSON object. A JSON array is not supported.</p>
evaluateFeature_evaluationContext :: Lens.Lens' EvaluateFeature (Prelude.Maybe Prelude.Text)
evaluateFeature_evaluationContext = Lens.lens (\EvaluateFeature' {evaluationContext} -> evaluationContext) (\s@EvaluateFeature' {} a -> s {evaluationContext = a} :: EvaluateFeature)

-- | An internal ID that represents a unique user of the application. This
-- @entityID@ is checked against any override rules assigned for this
-- feature.
evaluateFeature_entityId :: Lens.Lens' EvaluateFeature Prelude.Text
evaluateFeature_entityId = Lens.lens (\EvaluateFeature' {entityId} -> entityId) (\s@EvaluateFeature' {} a -> s {entityId = a} :: EvaluateFeature)

-- | The name of the feature being evaluated.
evaluateFeature_feature :: Lens.Lens' EvaluateFeature Prelude.Text
evaluateFeature_feature = Lens.lens (\EvaluateFeature' {feature} -> feature) (\s@EvaluateFeature' {} a -> s {feature = a} :: EvaluateFeature)

-- | The name or ARN of the project that contains this feature.
evaluateFeature_project :: Lens.Lens' EvaluateFeature Prelude.Text
evaluateFeature_project = Lens.lens (\EvaluateFeature' {project} -> project) (\s@EvaluateFeature' {} a -> s {project = a} :: EvaluateFeature)

instance Core.AWSRequest EvaluateFeature where
  type
    AWSResponse EvaluateFeature =
      EvaluateFeatureResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateFeatureResponse'
            Prelude.<$> (x Data..?> "variation")
            Prelude.<*> (x Data..?> "details")
            Prelude.<*> (x Data..?> "reason")
            Prelude.<*> (x Data..?> "value")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EvaluateFeature where
  hashWithSalt _salt EvaluateFeature' {..} =
    _salt `Prelude.hashWithSalt` evaluationContext
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` project

instance Prelude.NFData EvaluateFeature where
  rnf EvaluateFeature' {..} =
    Prelude.rnf evaluationContext
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf feature
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders EvaluateFeature where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EvaluateFeature where
  toJSON EvaluateFeature' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("evaluationContext" Data..=)
              Prelude.<$> evaluationContext,
            Prelude.Just ("entityId" Data..= entityId)
          ]
      )

instance Data.ToPath EvaluateFeature where
  toPath EvaluateFeature' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/evaluations/",
        Data.toBS feature
      ]

instance Data.ToQuery EvaluateFeature where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEvaluateFeatureResponse' smart constructor.
data EvaluateFeatureResponse = EvaluateFeatureResponse'
  { -- | The name of the variation that was served to the user session.
    variation :: Prelude.Maybe Prelude.Text,
    -- | If this user was assigned to a launch or experiment, this field lists
    -- the launch or experiment name.
    details :: Prelude.Maybe Prelude.Text,
    -- | Specifies the reason that the user session was assigned this variation.
    -- Possible values include @DEFAULT@, meaning the user was served the
    -- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
    -- in a launch; @EXPERIMENT_RULE_MATCH@, if the user session was enrolled
    -- in an experiment; or @ENTITY_OVERRIDES_MATCH@, if the user\'s @entityId@
    -- matches an override rule.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The value assigned to this variation to differentiate it from the other
    -- variations of this feature.
    value :: Prelude.Maybe VariableValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateFeatureResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variation', 'evaluateFeatureResponse_variation' - The name of the variation that was served to the user session.
--
-- 'details', 'evaluateFeatureResponse_details' - If this user was assigned to a launch or experiment, this field lists
-- the launch or experiment name.
--
-- 'reason', 'evaluateFeatureResponse_reason' - Specifies the reason that the user session was assigned this variation.
-- Possible values include @DEFAULT@, meaning the user was served the
-- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
-- in a launch; @EXPERIMENT_RULE_MATCH@, if the user session was enrolled
-- in an experiment; or @ENTITY_OVERRIDES_MATCH@, if the user\'s @entityId@
-- matches an override rule.
--
-- 'value', 'evaluateFeatureResponse_value' - The value assigned to this variation to differentiate it from the other
-- variations of this feature.
--
-- 'httpStatus', 'evaluateFeatureResponse_httpStatus' - The response's http status code.
newEvaluateFeatureResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EvaluateFeatureResponse
newEvaluateFeatureResponse pHttpStatus_ =
  EvaluateFeatureResponse'
    { variation =
        Prelude.Nothing,
      details = Prelude.Nothing,
      reason = Prelude.Nothing,
      value = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the variation that was served to the user session.
evaluateFeatureResponse_variation :: Lens.Lens' EvaluateFeatureResponse (Prelude.Maybe Prelude.Text)
evaluateFeatureResponse_variation = Lens.lens (\EvaluateFeatureResponse' {variation} -> variation) (\s@EvaluateFeatureResponse' {} a -> s {variation = a} :: EvaluateFeatureResponse)

-- | If this user was assigned to a launch or experiment, this field lists
-- the launch or experiment name.
evaluateFeatureResponse_details :: Lens.Lens' EvaluateFeatureResponse (Prelude.Maybe Prelude.Text)
evaluateFeatureResponse_details = Lens.lens (\EvaluateFeatureResponse' {details} -> details) (\s@EvaluateFeatureResponse' {} a -> s {details = a} :: EvaluateFeatureResponse)

-- | Specifies the reason that the user session was assigned this variation.
-- Possible values include @DEFAULT@, meaning the user was served the
-- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
-- in a launch; @EXPERIMENT_RULE_MATCH@, if the user session was enrolled
-- in an experiment; or @ENTITY_OVERRIDES_MATCH@, if the user\'s @entityId@
-- matches an override rule.
evaluateFeatureResponse_reason :: Lens.Lens' EvaluateFeatureResponse (Prelude.Maybe Prelude.Text)
evaluateFeatureResponse_reason = Lens.lens (\EvaluateFeatureResponse' {reason} -> reason) (\s@EvaluateFeatureResponse' {} a -> s {reason = a} :: EvaluateFeatureResponse)

-- | The value assigned to this variation to differentiate it from the other
-- variations of this feature.
evaluateFeatureResponse_value :: Lens.Lens' EvaluateFeatureResponse (Prelude.Maybe VariableValue)
evaluateFeatureResponse_value = Lens.lens (\EvaluateFeatureResponse' {value} -> value) (\s@EvaluateFeatureResponse' {} a -> s {value = a} :: EvaluateFeatureResponse)

-- | The response's http status code.
evaluateFeatureResponse_httpStatus :: Lens.Lens' EvaluateFeatureResponse Prelude.Int
evaluateFeatureResponse_httpStatus = Lens.lens (\EvaluateFeatureResponse' {httpStatus} -> httpStatus) (\s@EvaluateFeatureResponse' {} a -> s {httpStatus = a} :: EvaluateFeatureResponse)

instance Prelude.NFData EvaluateFeatureResponse where
  rnf EvaluateFeatureResponse' {..} =
    Prelude.rnf variation
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf httpStatus
