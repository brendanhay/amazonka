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
-- Module      : Network.AWS.IAM.Types.Statement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Statement where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.PolicySourceType
import Network.AWS.IAM.Types.Position
import qualified Network.AWS.Lens as Lens

-- | Contains a reference to a @Statement@ element in a policy document that
-- determines the result of the simulation.
--
-- This data type is used by the @MatchedStatements@ member of the
-- @ EvaluationResult @ type.
--
-- /See:/ 'newStatement' smart constructor.
data Statement = Statement'
  { -- | The row and column of the beginning of the @Statement@ in an IAM policy.
    startPosition :: Core.Maybe Position,
    -- | The type of the policy.
    sourcePolicyType :: Core.Maybe PolicySourceType,
    -- | The row and column of the end of a @Statement@ in an IAM policy.
    endPosition :: Core.Maybe Position,
    -- | The identifier of the policy that was provided as an input.
    sourcePolicyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Statement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startPosition', 'statement_startPosition' - The row and column of the beginning of the @Statement@ in an IAM policy.
--
-- 'sourcePolicyType', 'statement_sourcePolicyType' - The type of the policy.
--
-- 'endPosition', 'statement_endPosition' - The row and column of the end of a @Statement@ in an IAM policy.
--
-- 'sourcePolicyId', 'statement_sourcePolicyId' - The identifier of the policy that was provided as an input.
newStatement ::
  Statement
newStatement =
  Statement'
    { startPosition = Core.Nothing,
      sourcePolicyType = Core.Nothing,
      endPosition = Core.Nothing,
      sourcePolicyId = Core.Nothing
    }

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
statement_startPosition :: Lens.Lens' Statement (Core.Maybe Position)
statement_startPosition = Lens.lens (\Statement' {startPosition} -> startPosition) (\s@Statement' {} a -> s {startPosition = a} :: Statement)

-- | The type of the policy.
statement_sourcePolicyType :: Lens.Lens' Statement (Core.Maybe PolicySourceType)
statement_sourcePolicyType = Lens.lens (\Statement' {sourcePolicyType} -> sourcePolicyType) (\s@Statement' {} a -> s {sourcePolicyType = a} :: Statement)

-- | The row and column of the end of a @Statement@ in an IAM policy.
statement_endPosition :: Lens.Lens' Statement (Core.Maybe Position)
statement_endPosition = Lens.lens (\Statement' {endPosition} -> endPosition) (\s@Statement' {} a -> s {endPosition = a} :: Statement)

-- | The identifier of the policy that was provided as an input.
statement_sourcePolicyId :: Lens.Lens' Statement (Core.Maybe Core.Text)
statement_sourcePolicyId = Lens.lens (\Statement' {sourcePolicyId} -> sourcePolicyId) (\s@Statement' {} a -> s {sourcePolicyId = a} :: Statement)

instance Core.FromXML Statement where
  parseXML x =
    Statement'
      Core.<$> (x Core..@? "StartPosition")
      Core.<*> (x Core..@? "SourcePolicyType")
      Core.<*> (x Core..@? "EndPosition")
      Core.<*> (x Core..@? "SourcePolicyId")

instance Core.Hashable Statement

instance Core.NFData Statement
