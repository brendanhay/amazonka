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
-- Module      : Amazonka.Config.Types.EvaluationContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use EvaluationContext to group independently initiated proactive
-- resource evaluations. For example, CFN Stack. If you want to check just
-- a resource definition, you do not need to provide evaluation context.
--
-- /See:/ 'newEvaluationContext' smart constructor.
data EvaluationContext = EvaluationContext'
  { -- | A unique EvaluationContextIdentifier ID for an EvaluationContext.
    evaluationContextIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationContextIdentifier', 'evaluationContext_evaluationContextIdentifier' - A unique EvaluationContextIdentifier ID for an EvaluationContext.
newEvaluationContext ::
  EvaluationContext
newEvaluationContext =
  EvaluationContext'
    { evaluationContextIdentifier =
        Prelude.Nothing
    }

-- | A unique EvaluationContextIdentifier ID for an EvaluationContext.
evaluationContext_evaluationContextIdentifier :: Lens.Lens' EvaluationContext (Prelude.Maybe Prelude.Text)
evaluationContext_evaluationContextIdentifier = Lens.lens (\EvaluationContext' {evaluationContextIdentifier} -> evaluationContextIdentifier) (\s@EvaluationContext' {} a -> s {evaluationContextIdentifier = a} :: EvaluationContext)

instance Data.FromJSON EvaluationContext where
  parseJSON =
    Data.withObject
      "EvaluationContext"
      ( \x ->
          EvaluationContext'
            Prelude.<$> (x Data..:? "EvaluationContextIdentifier")
      )

instance Prelude.Hashable EvaluationContext where
  hashWithSalt _salt EvaluationContext' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationContextIdentifier

instance Prelude.NFData EvaluationContext where
  rnf EvaluationContext' {..} =
    Prelude.rnf evaluationContextIdentifier

instance Data.ToJSON EvaluationContext where
  toJSON EvaluationContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EvaluationContextIdentifier" Data..=)
              Prelude.<$> evaluationContextIdentifier
          ]
      )
