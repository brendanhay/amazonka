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
-- Module      : Network.AWS.Config.Types.EvaluationResultIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultIdentifier where

import Network.AWS.Config.Types.EvaluationResultQualifier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Uniquely identifies an evaluation result.
--
-- /See:/ 'newEvaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
  { -- | Identifies an Config rule used to evaluate an Amazon Web Services
    -- resource, and provides the type and ID of the evaluated resource.
    evaluationResultQualifier :: Prelude.Maybe EvaluationResultQualifier,
    -- | The time of the event that triggered the evaluation of your Amazon Web
    -- Services resources. The time can indicate when Config delivered a
    -- configuration item change notification, or it can indicate when Config
    -- delivered the configuration snapshot, depending on which event triggered
    -- the evaluation.
    orderingTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResultIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationResultQualifier', 'evaluationResultIdentifier_evaluationResultQualifier' - Identifies an Config rule used to evaluate an Amazon Web Services
-- resource, and provides the type and ID of the evaluated resource.
--
-- 'orderingTimestamp', 'evaluationResultIdentifier_orderingTimestamp' - The time of the event that triggered the evaluation of your Amazon Web
-- Services resources. The time can indicate when Config delivered a
-- configuration item change notification, or it can indicate when Config
-- delivered the configuration snapshot, depending on which event triggered
-- the evaluation.
newEvaluationResultIdentifier ::
  EvaluationResultIdentifier
newEvaluationResultIdentifier =
  EvaluationResultIdentifier'
    { evaluationResultQualifier =
        Prelude.Nothing,
      orderingTimestamp = Prelude.Nothing
    }

-- | Identifies an Config rule used to evaluate an Amazon Web Services
-- resource, and provides the type and ID of the evaluated resource.
evaluationResultIdentifier_evaluationResultQualifier :: Lens.Lens' EvaluationResultIdentifier (Prelude.Maybe EvaluationResultQualifier)
evaluationResultIdentifier_evaluationResultQualifier = Lens.lens (\EvaluationResultIdentifier' {evaluationResultQualifier} -> evaluationResultQualifier) (\s@EvaluationResultIdentifier' {} a -> s {evaluationResultQualifier = a} :: EvaluationResultIdentifier)

-- | The time of the event that triggered the evaluation of your Amazon Web
-- Services resources. The time can indicate when Config delivered a
-- configuration item change notification, or it can indicate when Config
-- delivered the configuration snapshot, depending on which event triggered
-- the evaluation.
evaluationResultIdentifier_orderingTimestamp :: Lens.Lens' EvaluationResultIdentifier (Prelude.Maybe Prelude.UTCTime)
evaluationResultIdentifier_orderingTimestamp = Lens.lens (\EvaluationResultIdentifier' {orderingTimestamp} -> orderingTimestamp) (\s@EvaluationResultIdentifier' {} a -> s {orderingTimestamp = a} :: EvaluationResultIdentifier) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON EvaluationResultIdentifier where
  parseJSON =
    Core.withObject
      "EvaluationResultIdentifier"
      ( \x ->
          EvaluationResultIdentifier'
            Prelude.<$> (x Core..:? "EvaluationResultQualifier")
            Prelude.<*> (x Core..:? "OrderingTimestamp")
      )

instance Prelude.Hashable EvaluationResultIdentifier

instance Prelude.NFData EvaluationResultIdentifier
