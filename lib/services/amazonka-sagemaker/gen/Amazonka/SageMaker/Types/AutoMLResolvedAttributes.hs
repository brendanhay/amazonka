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
-- Module      : Amazonka.SageMaker.Types.AutoMLResolvedAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLResolvedAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLJobObjective
import Amazonka.SageMaker.Types.AutoMLProblemTypeResolvedAttributes

-- | The resolved attributes used to configure an AutoML job V2.
--
-- /See:/ 'newAutoMLResolvedAttributes' smart constructor.
data AutoMLResolvedAttributes = AutoMLResolvedAttributes'
  { autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | Defines the resolved attributes specific to a problem type.
    autoMLProblemTypeResolvedAttributes :: Prelude.Maybe AutoMLProblemTypeResolvedAttributes,
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLResolvedAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobObjective', 'autoMLResolvedAttributes_autoMLJobObjective' - Undocumented member.
--
-- 'autoMLProblemTypeResolvedAttributes', 'autoMLResolvedAttributes_autoMLProblemTypeResolvedAttributes' - Defines the resolved attributes specific to a problem type.
--
-- 'completionCriteria', 'autoMLResolvedAttributes_completionCriteria' - Undocumented member.
newAutoMLResolvedAttributes ::
  AutoMLResolvedAttributes
newAutoMLResolvedAttributes =
  AutoMLResolvedAttributes'
    { autoMLJobObjective =
        Prelude.Nothing,
      autoMLProblemTypeResolvedAttributes =
        Prelude.Nothing,
      completionCriteria = Prelude.Nothing
    }

-- | Undocumented member.
autoMLResolvedAttributes_autoMLJobObjective :: Lens.Lens' AutoMLResolvedAttributes (Prelude.Maybe AutoMLJobObjective)
autoMLResolvedAttributes_autoMLJobObjective = Lens.lens (\AutoMLResolvedAttributes' {autoMLJobObjective} -> autoMLJobObjective) (\s@AutoMLResolvedAttributes' {} a -> s {autoMLJobObjective = a} :: AutoMLResolvedAttributes)

-- | Defines the resolved attributes specific to a problem type.
autoMLResolvedAttributes_autoMLProblemTypeResolvedAttributes :: Lens.Lens' AutoMLResolvedAttributes (Prelude.Maybe AutoMLProblemTypeResolvedAttributes)
autoMLResolvedAttributes_autoMLProblemTypeResolvedAttributes = Lens.lens (\AutoMLResolvedAttributes' {autoMLProblemTypeResolvedAttributes} -> autoMLProblemTypeResolvedAttributes) (\s@AutoMLResolvedAttributes' {} a -> s {autoMLProblemTypeResolvedAttributes = a} :: AutoMLResolvedAttributes)

-- | Undocumented member.
autoMLResolvedAttributes_completionCriteria :: Lens.Lens' AutoMLResolvedAttributes (Prelude.Maybe AutoMLJobCompletionCriteria)
autoMLResolvedAttributes_completionCriteria = Lens.lens (\AutoMLResolvedAttributes' {completionCriteria} -> completionCriteria) (\s@AutoMLResolvedAttributes' {} a -> s {completionCriteria = a} :: AutoMLResolvedAttributes)

instance Data.FromJSON AutoMLResolvedAttributes where
  parseJSON =
    Data.withObject
      "AutoMLResolvedAttributes"
      ( \x ->
          AutoMLResolvedAttributes'
            Prelude.<$> (x Data..:? "AutoMLJobObjective")
            Prelude.<*> (x Data..:? "AutoMLProblemTypeResolvedAttributes")
            Prelude.<*> (x Data..:? "CompletionCriteria")
      )

instance Prelude.Hashable AutoMLResolvedAttributes where
  hashWithSalt _salt AutoMLResolvedAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` autoMLJobObjective
      `Prelude.hashWithSalt` autoMLProblemTypeResolvedAttributes
      `Prelude.hashWithSalt` completionCriteria

instance Prelude.NFData AutoMLResolvedAttributes where
  rnf AutoMLResolvedAttributes' {..} =
    Prelude.rnf autoMLJobObjective
      `Prelude.seq` Prelude.rnf autoMLProblemTypeResolvedAttributes
      `Prelude.seq` Prelude.rnf completionCriteria
