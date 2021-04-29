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
-- Module      : Network.AWS.Config.Types.EvaluationResultQualifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultQualifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies an AWS Config rule that evaluated an AWS resource, and
-- provides the type and ID of the resource that the rule evaluated.
--
-- /See:/ 'newEvaluationResultQualifier' smart constructor.
data EvaluationResultQualifier = EvaluationResultQualifier'
  { -- | The ID of the evaluated AWS resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Config rule that was used in the evaluation.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS resource that was evaluated.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResultQualifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'evaluationResultQualifier_resourceId' - The ID of the evaluated AWS resource.
--
-- 'configRuleName', 'evaluationResultQualifier_configRuleName' - The name of the AWS Config rule that was used in the evaluation.
--
-- 'resourceType', 'evaluationResultQualifier_resourceType' - The type of AWS resource that was evaluated.
newEvaluationResultQualifier ::
  EvaluationResultQualifier
newEvaluationResultQualifier =
  EvaluationResultQualifier'
    { resourceId =
        Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The ID of the evaluated AWS resource.
evaluationResultQualifier_resourceId :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_resourceId = Lens.lens (\EvaluationResultQualifier' {resourceId} -> resourceId) (\s@EvaluationResultQualifier' {} a -> s {resourceId = a} :: EvaluationResultQualifier)

-- | The name of the AWS Config rule that was used in the evaluation.
evaluationResultQualifier_configRuleName :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_configRuleName = Lens.lens (\EvaluationResultQualifier' {configRuleName} -> configRuleName) (\s@EvaluationResultQualifier' {} a -> s {configRuleName = a} :: EvaluationResultQualifier)

-- | The type of AWS resource that was evaluated.
evaluationResultQualifier_resourceType :: Lens.Lens' EvaluationResultQualifier (Prelude.Maybe Prelude.Text)
evaluationResultQualifier_resourceType = Lens.lens (\EvaluationResultQualifier' {resourceType} -> resourceType) (\s@EvaluationResultQualifier' {} a -> s {resourceType = a} :: EvaluationResultQualifier)

instance Prelude.FromJSON EvaluationResultQualifier where
  parseJSON =
    Prelude.withObject
      "EvaluationResultQualifier"
      ( \x ->
          EvaluationResultQualifier'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "ConfigRuleName")
            Prelude.<*> (x Prelude..:? "ResourceType")
      )

instance Prelude.Hashable EvaluationResultQualifier

instance Prelude.NFData EvaluationResultQualifier
