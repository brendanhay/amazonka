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
-- Module      : Amazonka.Evidently.Types.EvaluationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.EvaluationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.VariableValue
import qualified Amazonka.Prelude as Prelude

-- | This structure displays the results of one feature evaluation assignment
-- to one user session.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | If this user was assigned to a launch or experiment, this field lists
    -- the launch or experiment name.
    details :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the project that contains the feature being
    -- evaluated.
    project :: Prelude.Maybe Prelude.Text,
    -- | Specifies the reason that the user session was assigned this variation.
    -- Possible values include @DEFAULT@, meaning the user was served the
    -- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
    -- in a launch; or @EXPERIMENT_RULE_MATCH@, if the user session was
    -- enrolled in an experiment.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The value assigned to this variation to differentiate it from the other
    -- variations of this feature.
    value :: Prelude.Maybe VariableValue,
    -- | The name of the variation that was served to the user session.
    variation :: Prelude.Maybe Prelude.Text,
    -- | An internal ID that represents a unique user session of the application.
    entityId :: Prelude.Text,
    -- | The name of the feature being evaluated.
    feature :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'evaluationResult_details' - If this user was assigned to a launch or experiment, this field lists
-- the launch or experiment name.
--
-- 'project', 'evaluationResult_project' - The name or ARN of the project that contains the feature being
-- evaluated.
--
-- 'reason', 'evaluationResult_reason' - Specifies the reason that the user session was assigned this variation.
-- Possible values include @DEFAULT@, meaning the user was served the
-- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
-- in a launch; or @EXPERIMENT_RULE_MATCH@, if the user session was
-- enrolled in an experiment.
--
-- 'value', 'evaluationResult_value' - The value assigned to this variation to differentiate it from the other
-- variations of this feature.
--
-- 'variation', 'evaluationResult_variation' - The name of the variation that was served to the user session.
--
-- 'entityId', 'evaluationResult_entityId' - An internal ID that represents a unique user session of the application.
--
-- 'feature', 'evaluationResult_feature' - The name of the feature being evaluated.
newEvaluationResult ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'feature'
  Prelude.Text ->
  EvaluationResult
newEvaluationResult pEntityId_ pFeature_ =
  EvaluationResult'
    { details = Prelude.Nothing,
      project = Prelude.Nothing,
      reason = Prelude.Nothing,
      value = Prelude.Nothing,
      variation = Prelude.Nothing,
      entityId = pEntityId_,
      feature = pFeature_
    }

-- | If this user was assigned to a launch or experiment, this field lists
-- the launch or experiment name.
evaluationResult_details :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_details = Lens.lens (\EvaluationResult' {details} -> details) (\s@EvaluationResult' {} a -> s {details = a} :: EvaluationResult)

-- | The name or ARN of the project that contains the feature being
-- evaluated.
evaluationResult_project :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_project = Lens.lens (\EvaluationResult' {project} -> project) (\s@EvaluationResult' {} a -> s {project = a} :: EvaluationResult)

-- | Specifies the reason that the user session was assigned this variation.
-- Possible values include @DEFAULT@, meaning the user was served the
-- default variation; @LAUNCH_RULE_MATCH@, if the user session was enrolled
-- in a launch; or @EXPERIMENT_RULE_MATCH@, if the user session was
-- enrolled in an experiment.
evaluationResult_reason :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_reason = Lens.lens (\EvaluationResult' {reason} -> reason) (\s@EvaluationResult' {} a -> s {reason = a} :: EvaluationResult)

-- | The value assigned to this variation to differentiate it from the other
-- variations of this feature.
evaluationResult_value :: Lens.Lens' EvaluationResult (Prelude.Maybe VariableValue)
evaluationResult_value = Lens.lens (\EvaluationResult' {value} -> value) (\s@EvaluationResult' {} a -> s {value = a} :: EvaluationResult)

-- | The name of the variation that was served to the user session.
evaluationResult_variation :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_variation = Lens.lens (\EvaluationResult' {variation} -> variation) (\s@EvaluationResult' {} a -> s {variation = a} :: EvaluationResult)

-- | An internal ID that represents a unique user session of the application.
evaluationResult_entityId :: Lens.Lens' EvaluationResult Prelude.Text
evaluationResult_entityId = Lens.lens (\EvaluationResult' {entityId} -> entityId) (\s@EvaluationResult' {} a -> s {entityId = a} :: EvaluationResult)

-- | The name of the feature being evaluated.
evaluationResult_feature :: Lens.Lens' EvaluationResult Prelude.Text
evaluationResult_feature = Lens.lens (\EvaluationResult' {feature} -> feature) (\s@EvaluationResult' {} a -> s {feature = a} :: EvaluationResult)

instance Data.FromJSON EvaluationResult where
  parseJSON =
    Data.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Data..:? "details")
            Prelude.<*> (x Data..:? "project")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "value")
            Prelude.<*> (x Data..:? "variation")
            Prelude.<*> (x Data..: "entityId")
            Prelude.<*> (x Data..: "feature")
      )

instance Prelude.Hashable EvaluationResult where
  hashWithSalt _salt EvaluationResult' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` variation
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` feature

instance Prelude.NFData EvaluationResult where
  rnf EvaluationResult' {..} =
    Prelude.rnf details `Prelude.seq`
      Prelude.rnf project `Prelude.seq`
        Prelude.rnf reason `Prelude.seq`
          Prelude.rnf value `Prelude.seq`
            Prelude.rnf variation `Prelude.seq`
              Prelude.rnf entityId `Prelude.seq`
                Prelude.rnf feature
