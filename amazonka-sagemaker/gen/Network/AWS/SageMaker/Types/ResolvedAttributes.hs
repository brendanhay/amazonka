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
-- Module      : Network.AWS.SageMaker.Types.ResolvedAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResolvedAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLJobObjective
import Network.AWS.SageMaker.Types.ProblemType

-- | The resolved attributes.
--
-- /See:/ 'newResolvedAttributes' smart constructor.
data ResolvedAttributes = ResolvedAttributes'
  { completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria,
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | The problem type.
    problemType :: Prelude.Maybe ProblemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResolvedAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionCriteria', 'resolvedAttributes_completionCriteria' - Undocumented member.
--
-- 'autoMLJobObjective', 'resolvedAttributes_autoMLJobObjective' - Undocumented member.
--
-- 'problemType', 'resolvedAttributes_problemType' - The problem type.
newResolvedAttributes ::
  ResolvedAttributes
newResolvedAttributes =
  ResolvedAttributes'
    { completionCriteria =
        Prelude.Nothing,
      autoMLJobObjective = Prelude.Nothing,
      problemType = Prelude.Nothing
    }

-- | Undocumented member.
resolvedAttributes_completionCriteria :: Lens.Lens' ResolvedAttributes (Prelude.Maybe AutoMLJobCompletionCriteria)
resolvedAttributes_completionCriteria = Lens.lens (\ResolvedAttributes' {completionCriteria} -> completionCriteria) (\s@ResolvedAttributes' {} a -> s {completionCriteria = a} :: ResolvedAttributes)

-- | Undocumented member.
resolvedAttributes_autoMLJobObjective :: Lens.Lens' ResolvedAttributes (Prelude.Maybe AutoMLJobObjective)
resolvedAttributes_autoMLJobObjective = Lens.lens (\ResolvedAttributes' {autoMLJobObjective} -> autoMLJobObjective) (\s@ResolvedAttributes' {} a -> s {autoMLJobObjective = a} :: ResolvedAttributes)

-- | The problem type.
resolvedAttributes_problemType :: Lens.Lens' ResolvedAttributes (Prelude.Maybe ProblemType)
resolvedAttributes_problemType = Lens.lens (\ResolvedAttributes' {problemType} -> problemType) (\s@ResolvedAttributes' {} a -> s {problemType = a} :: ResolvedAttributes)

instance Prelude.FromJSON ResolvedAttributes where
  parseJSON =
    Prelude.withObject
      "ResolvedAttributes"
      ( \x ->
          ResolvedAttributes'
            Prelude.<$> (x Prelude..:? "CompletionCriteria")
            Prelude.<*> (x Prelude..:? "AutoMLJobObjective")
            Prelude.<*> (x Prelude..:? "ProblemType")
      )

instance Prelude.Hashable ResolvedAttributes

instance Prelude.NFData ResolvedAttributes
