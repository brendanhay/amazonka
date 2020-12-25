{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ParameterizedStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ParameterizedStatement
  ( ParameterizedStatement (..),

    -- * Smart constructor
    mkParameterizedStatement,

    -- * Lenses
    psStatement,
    psParameters,
  )
where

import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.Statement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a PartiQL statment that uses parameters.
--
-- /See:/ 'mkParameterizedStatement' smart constructor.
data ParameterizedStatement = ParameterizedStatement'
  { -- | A PartiQL statment that uses parameters.
    statement :: Types.Statement,
    -- | The parameter values.
    parameters :: Core.Maybe (Core.NonEmpty Types.AttributeValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterizedStatement' value with any optional fields omitted.
mkParameterizedStatement ::
  -- | 'statement'
  Types.Statement ->
  ParameterizedStatement
mkParameterizedStatement statement =
  ParameterizedStatement' {statement, parameters = Core.Nothing}

-- | A PartiQL statment that uses parameters.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psStatement :: Lens.Lens' ParameterizedStatement Types.Statement
psStatement = Lens.field @"statement"
{-# DEPRECATED psStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | The parameter values.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParameters :: Lens.Lens' ParameterizedStatement (Core.Maybe (Core.NonEmpty Types.AttributeValue))
psParameters = Lens.field @"parameters"
{-# DEPRECATED psParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON ParameterizedStatement where
  toJSON ParameterizedStatement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Statement" Core..= statement),
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )
