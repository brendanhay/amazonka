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
-- Module      : Amazonka.Config.Types.EvaluationResultIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationResultIdentifier where

import Amazonka.Config.Types.EvaluationResultQualifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Uniquely identifies an evaluation result.
--
-- /See:/ 'newEvaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
  { -- | The time of the event that triggered the evaluation of your Amazon Web
    -- Services resources. The time can indicate when Config delivered a
    -- configuration item change notification, or it can indicate when Config
    -- delivered the configuration snapshot, depending on which event triggered
    -- the evaluation.
    orderingTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Identifies an Config rule used to evaluate an Amazon Web Services
    -- resource, and provides the type and ID of the evaluated resource.
    evaluationResultQualifier :: Prelude.Maybe EvaluationResultQualifier
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
-- 'orderingTimestamp', 'evaluationResultIdentifier_orderingTimestamp' - The time of the event that triggered the evaluation of your Amazon Web
-- Services resources. The time can indicate when Config delivered a
-- configuration item change notification, or it can indicate when Config
-- delivered the configuration snapshot, depending on which event triggered
-- the evaluation.
--
-- 'evaluationResultQualifier', 'evaluationResultIdentifier_evaluationResultQualifier' - Identifies an Config rule used to evaluate an Amazon Web Services
-- resource, and provides the type and ID of the evaluated resource.
newEvaluationResultIdentifier ::
  EvaluationResultIdentifier
newEvaluationResultIdentifier =
  EvaluationResultIdentifier'
    { orderingTimestamp =
        Prelude.Nothing,
      evaluationResultQualifier = Prelude.Nothing
    }

-- | The time of the event that triggered the evaluation of your Amazon Web
-- Services resources. The time can indicate when Config delivered a
-- configuration item change notification, or it can indicate when Config
-- delivered the configuration snapshot, depending on which event triggered
-- the evaluation.
evaluationResultIdentifier_orderingTimestamp :: Lens.Lens' EvaluationResultIdentifier (Prelude.Maybe Prelude.UTCTime)
evaluationResultIdentifier_orderingTimestamp = Lens.lens (\EvaluationResultIdentifier' {orderingTimestamp} -> orderingTimestamp) (\s@EvaluationResultIdentifier' {} a -> s {orderingTimestamp = a} :: EvaluationResultIdentifier) Prelude.. Lens.mapping Core._Time

-- | Identifies an Config rule used to evaluate an Amazon Web Services
-- resource, and provides the type and ID of the evaluated resource.
evaluationResultIdentifier_evaluationResultQualifier :: Lens.Lens' EvaluationResultIdentifier (Prelude.Maybe EvaluationResultQualifier)
evaluationResultIdentifier_evaluationResultQualifier = Lens.lens (\EvaluationResultIdentifier' {evaluationResultQualifier} -> evaluationResultQualifier) (\s@EvaluationResultIdentifier' {} a -> s {evaluationResultQualifier = a} :: EvaluationResultIdentifier)

instance Core.FromJSON EvaluationResultIdentifier where
  parseJSON =
    Core.withObject
      "EvaluationResultIdentifier"
      ( \x ->
          EvaluationResultIdentifier'
            Prelude.<$> (x Core..:? "OrderingTimestamp")
            Prelude.<*> (x Core..:? "EvaluationResultQualifier")
      )

instance Prelude.Hashable EvaluationResultIdentifier where
  hashWithSalt _salt EvaluationResultIdentifier' {..} =
    _salt `Prelude.hashWithSalt` orderingTimestamp
      `Prelude.hashWithSalt` evaluationResultQualifier

instance Prelude.NFData EvaluationResultIdentifier where
  rnf EvaluationResultIdentifier' {..} =
    Prelude.rnf orderingTimestamp
      `Prelude.seq` Prelude.rnf evaluationResultQualifier
