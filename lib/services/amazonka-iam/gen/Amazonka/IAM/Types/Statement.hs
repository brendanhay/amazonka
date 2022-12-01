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
-- Module      : Amazonka.IAM.Types.Statement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.Statement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types.PolicySourceType
import Amazonka.IAM.Types.Position
import qualified Amazonka.Prelude as Prelude

-- | Contains a reference to a @Statement@ element in a policy document that
-- determines the result of the simulation.
--
-- This data type is used by the @MatchedStatements@ member of the
-- @ EvaluationResult @ type.
--
-- /See:/ 'newStatement' smart constructor.
data Statement = Statement'
  { -- | The identifier of the policy that was provided as an input.
    sourcePolicyId :: Prelude.Maybe Prelude.Text,
    -- | The row and column of the beginning of the @Statement@ in an IAM policy.
    startPosition :: Prelude.Maybe Position,
    -- | The row and column of the end of a @Statement@ in an IAM policy.
    endPosition :: Prelude.Maybe Position,
    -- | The type of the policy.
    sourcePolicyType :: Prelude.Maybe PolicySourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePolicyId', 'statement_sourcePolicyId' - The identifier of the policy that was provided as an input.
--
-- 'startPosition', 'statement_startPosition' - The row and column of the beginning of the @Statement@ in an IAM policy.
--
-- 'endPosition', 'statement_endPosition' - The row and column of the end of a @Statement@ in an IAM policy.
--
-- 'sourcePolicyType', 'statement_sourcePolicyType' - The type of the policy.
newStatement ::
  Statement
newStatement =
  Statement'
    { sourcePolicyId = Prelude.Nothing,
      startPosition = Prelude.Nothing,
      endPosition = Prelude.Nothing,
      sourcePolicyType = Prelude.Nothing
    }

-- | The identifier of the policy that was provided as an input.
statement_sourcePolicyId :: Lens.Lens' Statement (Prelude.Maybe Prelude.Text)
statement_sourcePolicyId = Lens.lens (\Statement' {sourcePolicyId} -> sourcePolicyId) (\s@Statement' {} a -> s {sourcePolicyId = a} :: Statement)

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
statement_startPosition :: Lens.Lens' Statement (Prelude.Maybe Position)
statement_startPosition = Lens.lens (\Statement' {startPosition} -> startPosition) (\s@Statement' {} a -> s {startPosition = a} :: Statement)

-- | The row and column of the end of a @Statement@ in an IAM policy.
statement_endPosition :: Lens.Lens' Statement (Prelude.Maybe Position)
statement_endPosition = Lens.lens (\Statement' {endPosition} -> endPosition) (\s@Statement' {} a -> s {endPosition = a} :: Statement)

-- | The type of the policy.
statement_sourcePolicyType :: Lens.Lens' Statement (Prelude.Maybe PolicySourceType)
statement_sourcePolicyType = Lens.lens (\Statement' {sourcePolicyType} -> sourcePolicyType) (\s@Statement' {} a -> s {sourcePolicyType = a} :: Statement)

instance Core.FromXML Statement where
  parseXML x =
    Statement'
      Prelude.<$> (x Core..@? "SourcePolicyId")
      Prelude.<*> (x Core..@? "StartPosition")
      Prelude.<*> (x Core..@? "EndPosition")
      Prelude.<*> (x Core..@? "SourcePolicyType")

instance Prelude.Hashable Statement where
  hashWithSalt _salt Statement' {..} =
    _salt `Prelude.hashWithSalt` sourcePolicyId
      `Prelude.hashWithSalt` startPosition
      `Prelude.hashWithSalt` endPosition
      `Prelude.hashWithSalt` sourcePolicyType

instance Prelude.NFData Statement where
  rnf Statement' {..} =
    Prelude.rnf sourcePolicyId
      `Prelude.seq` Prelude.rnf startPosition
      `Prelude.seq` Prelude.rnf endPosition
      `Prelude.seq` Prelude.rnf sourcePolicyType
