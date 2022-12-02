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
-- Module      : Amazonka.Evidently.Types.EvaluationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.EvaluationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure assigns a feature variation to one user session.
--
-- /See:/ 'newEvaluationRequest' smart constructor.
data EvaluationRequest = EvaluationRequest'
  { -- | A JSON block of attributes that you can optionally pass in. This JSON
    -- block is included in the evaluation events sent to Evidently from the
    -- user session.
    evaluationContext :: Prelude.Maybe Prelude.Text,
    -- | An internal ID that represents a unique user session of the application.
    -- This @entityID@ is checked against any override rules assigned for this
    -- feature.
    entityId :: Prelude.Text,
    -- | The name of the feature being evaluated.
    feature :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationContext', 'evaluationRequest_evaluationContext' - A JSON block of attributes that you can optionally pass in. This JSON
-- block is included in the evaluation events sent to Evidently from the
-- user session.
--
-- 'entityId', 'evaluationRequest_entityId' - An internal ID that represents a unique user session of the application.
-- This @entityID@ is checked against any override rules assigned for this
-- feature.
--
-- 'feature', 'evaluationRequest_feature' - The name of the feature being evaluated.
newEvaluationRequest ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'feature'
  Prelude.Text ->
  EvaluationRequest
newEvaluationRequest pEntityId_ pFeature_ =
  EvaluationRequest'
    { evaluationContext =
        Prelude.Nothing,
      entityId = pEntityId_,
      feature = pFeature_
    }

-- | A JSON block of attributes that you can optionally pass in. This JSON
-- block is included in the evaluation events sent to Evidently from the
-- user session.
evaluationRequest_evaluationContext :: Lens.Lens' EvaluationRequest (Prelude.Maybe Prelude.Text)
evaluationRequest_evaluationContext = Lens.lens (\EvaluationRequest' {evaluationContext} -> evaluationContext) (\s@EvaluationRequest' {} a -> s {evaluationContext = a} :: EvaluationRequest)

-- | An internal ID that represents a unique user session of the application.
-- This @entityID@ is checked against any override rules assigned for this
-- feature.
evaluationRequest_entityId :: Lens.Lens' EvaluationRequest Prelude.Text
evaluationRequest_entityId = Lens.lens (\EvaluationRequest' {entityId} -> entityId) (\s@EvaluationRequest' {} a -> s {entityId = a} :: EvaluationRequest)

-- | The name of the feature being evaluated.
evaluationRequest_feature :: Lens.Lens' EvaluationRequest Prelude.Text
evaluationRequest_feature = Lens.lens (\EvaluationRequest' {feature} -> feature) (\s@EvaluationRequest' {} a -> s {feature = a} :: EvaluationRequest)

instance Prelude.Hashable EvaluationRequest where
  hashWithSalt _salt EvaluationRequest' {..} =
    _salt `Prelude.hashWithSalt` evaluationContext
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` feature

instance Prelude.NFData EvaluationRequest where
  rnf EvaluationRequest' {..} =
    Prelude.rnf evaluationContext
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf feature

instance Data.ToJSON EvaluationRequest where
  toJSON EvaluationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("evaluationContext" Data..=)
              Prelude.<$> evaluationContext,
            Prelude.Just ("entityId" Data..= entityId),
            Prelude.Just ("feature" Data..= feature)
          ]
      )
