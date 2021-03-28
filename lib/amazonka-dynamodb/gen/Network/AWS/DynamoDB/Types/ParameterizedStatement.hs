{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ParameterizedStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ParameterizedStatement
  ( ParameterizedStatement (..)
  -- * Smart constructor
  , mkParameterizedStatement
  -- * Lenses
  , psStatement
  , psParameters
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDB.Types.Statement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a PartiQL statment that uses parameters. 
--
-- /See:/ 'mkParameterizedStatement' smart constructor.
data ParameterizedStatement = ParameterizedStatement'
  { statement :: Types.Statement
    -- ^ A PartiQL statment that uses parameters. 
  , parameters :: Core.Maybe (Core.NonEmpty Types.AttributeValue)
    -- ^ The parameter values. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterizedStatement' value with any optional fields omitted.
mkParameterizedStatement
    :: Types.Statement -- ^ 'statement'
    -> ParameterizedStatement
mkParameterizedStatement statement
  = ParameterizedStatement'{statement, parameters = Core.Nothing}

-- | A PartiQL statment that uses parameters. 
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psStatement :: Lens.Lens' ParameterizedStatement Types.Statement
psStatement = Lens.field @"statement"
{-# INLINEABLE psStatement #-}
{-# DEPRECATED statement "Use generic-lens or generic-optics with 'statement' instead"  #-}

-- | The parameter values. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParameters :: Lens.Lens' ParameterizedStatement (Core.Maybe (Core.NonEmpty Types.AttributeValue))
psParameters = Lens.field @"parameters"
{-# INLINEABLE psParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromJSON ParameterizedStatement where
        toJSON ParameterizedStatement{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Statement" Core..= statement),
                  ("Parameters" Core..=) Core.<$> parameters])
