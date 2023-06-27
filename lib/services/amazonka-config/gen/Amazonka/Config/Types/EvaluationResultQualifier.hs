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
-- Module      : Amazonka.Config.Types.EvaluationResultQualifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationResultQualifier where

import Amazonka.Config.Types.EvaluationMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies an Config rule that evaluated an Amazon Web Services
-- resource, and provides the type and ID of the resource that the rule
-- evaluated.
--
-- /See:/ 'newEvaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
  { -- | The name of the Config rule that was used in the evaluation.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | The mode of an evaluation. The valid values are Detective or Proactive.
    evaluationMode :: Prelude.Maybe EvaluationMode,
    -- | The ID of the evaluated Amazon Web Services resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource that was evaluated.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResultQualifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'evaluationResultQualifier_configRuleName' - The name of the Config rule that was used in the evaluation.
--
-- 'evaluationMode', 'evaluationResultQualifier_evaluationMode' - The mode of an evaluation. The valid values are Detective or Proactive.
--
-- 'resourceId', 'evaluationResultQualifier_resourceId' - The ID of the evaluated Amazon Web Services resource.
--
-- 'resourceType', 'evaluationResultQualifier_resourceType' - The type of Amazon Web Services resource that was evaluated.
newEvaluationResultQualifier ::
  EvaluationResultQualifier
newEvaluationResultQualifier =
  EvaluationResultQualifier'
    { configRuleName =
        Prelude.Nothing,
      evaluationMode = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The name of the Config rule that was used in the evaluation.
evaluationResultQualifier_configRuleName :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_configRuleName = Lens.lens (\EvaluationResultQualifier' {configRuleName} -> configRuleName) (\s@EvaluationResultQualifier' {} a -> s {configRuleName = a} :: EvaluationResultQualifier)

-- | The mode of an evaluation. The valid values are Detective or Proactive.
evaluationResultQualifier_evaluationMode :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe EvaluationMode)
evaluationResultQualifier_evaluationMode = Lens.lens (\EvaluationResultQualifier' {evaluationMode} -> evaluationMode) (\s@EvaluationResultQualifier' {} a -> s {evaluationMode = a} :: EvaluationResultQualifier)

-- | The ID of the evaluated Amazon Web Services resource.
evaluationResultQualifier_resourceId :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_resourceId = Lens.lens (\EvaluationResultQualifier' {resourceId} -> resourceId) (\s@EvaluationResultQualifier' {} a -> s {resourceId = a} :: EvaluationResultQualifier)

-- | The type of Amazon Web Services resource that was evaluated.
evaluationResultQualifier_resourceType :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_resourceType = Lens.lens (\EvaluationResultQualifier' {resourceType} -> resourceType) (\s@EvaluationResultQualifier' {} a -> s {resourceType = a} :: EvaluationResultQualifier)

instance Data.FromJSON EvaluationResultQualifier where
  parseJSON =
    Data.withObject
      "EvaluationResultQualifier"
      ( \x ->
          EvaluationResultQualifier'
            Prelude.<$> (x Data..:? "ConfigRuleName")
            Prelude.<*> (x Data..:? "EvaluationMode")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance Prelude.Hashable EvaluationResultQualifier where
  hashWithSalt _salt EvaluationResultQualifier' {..} =
    _salt
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` evaluationMode
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData EvaluationResultQualifier where
  rnf EvaluationResultQualifier' {..} =
    Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf evaluationMode
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
