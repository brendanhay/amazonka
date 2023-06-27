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
-- Module      : Amazonka.Evidently.Types.EvaluationRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.EvaluationRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the information about an evaluation rule for
-- this feature, if it is used in a launch or experiment.
--
-- /See:/ 'newEvaluationRule' smart constructor.
data EvaluationRule = EvaluationRule'
  { -- | The name of the experiment or launch.
    name :: Prelude.Maybe Prelude.Text,
    -- | This value is @aws.evidently.splits@ if this is an evaluation rule for a
    -- launch, and it is @aws.evidently.onlineab@ if this is an evaluation rule
    -- for an experiment.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'evaluationRule_name' - The name of the experiment or launch.
--
-- 'type'', 'evaluationRule_type' - This value is @aws.evidently.splits@ if this is an evaluation rule for a
-- launch, and it is @aws.evidently.onlineab@ if this is an evaluation rule
-- for an experiment.
newEvaluationRule ::
  -- | 'type''
  Prelude.Text ->
  EvaluationRule
newEvaluationRule pType_ =
  EvaluationRule'
    { name = Prelude.Nothing,
      type' = pType_
    }

-- | The name of the experiment or launch.
evaluationRule_name :: Lens.Lens' EvaluationRule (Prelude.Maybe Prelude.Text)
evaluationRule_name = Lens.lens (\EvaluationRule' {name} -> name) (\s@EvaluationRule' {} a -> s {name = a} :: EvaluationRule)

-- | This value is @aws.evidently.splits@ if this is an evaluation rule for a
-- launch, and it is @aws.evidently.onlineab@ if this is an evaluation rule
-- for an experiment.
evaluationRule_type :: Lens.Lens' EvaluationRule Prelude.Text
evaluationRule_type = Lens.lens (\EvaluationRule' {type'} -> type') (\s@EvaluationRule' {} a -> s {type' = a} :: EvaluationRule)

instance Data.FromJSON EvaluationRule where
  parseJSON =
    Data.withObject
      "EvaluationRule"
      ( \x ->
          EvaluationRule'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable EvaluationRule where
  hashWithSalt _salt EvaluationRule' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EvaluationRule where
  rnf EvaluationRule' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
