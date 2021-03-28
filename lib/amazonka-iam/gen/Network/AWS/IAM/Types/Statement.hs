{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Statement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Statement
  ( Statement (..)
  -- * Smart constructor
  , mkStatement
  -- * Lenses
  , sEndPosition
  , sSourcePolicyId
  , sSourcePolicyType
  , sStartPosition
  ) where

import qualified Network.AWS.IAM.Types.PolicySourceType as Types
import qualified Network.AWS.IAM.Types.Position as Types
import qualified Network.AWS.IAM.Types.SourcePolicyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a reference to a @Statement@ element in a policy document that determines the result of the simulation.
--
-- This data type is used by the @MatchedStatements@ member of the @'EvaluationResult' @ type.
--
-- /See:/ 'mkStatement' smart constructor.
data Statement = Statement'
  { endPosition :: Core.Maybe Types.Position
    -- ^ The row and column of the end of a @Statement@ in an IAM policy.
  , sourcePolicyId :: Core.Maybe Types.SourcePolicyId
    -- ^ The identifier of the policy that was provided as an input.
  , sourcePolicyType :: Core.Maybe Types.PolicySourceType
    -- ^ The type of the policy.
  , startPosition :: Core.Maybe Types.Position
    -- ^ The row and column of the beginning of the @Statement@ in an IAM policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Statement' value with any optional fields omitted.
mkStatement
    :: Statement
mkStatement
  = Statement'{endPosition = Core.Nothing,
               sourcePolicyId = Core.Nothing, sourcePolicyType = Core.Nothing,
               startPosition = Core.Nothing}

-- | The row and column of the end of a @Statement@ in an IAM policy.
--
-- /Note:/ Consider using 'endPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndPosition :: Lens.Lens' Statement (Core.Maybe Types.Position)
sEndPosition = Lens.field @"endPosition"
{-# INLINEABLE sEndPosition #-}
{-# DEPRECATED endPosition "Use generic-lens or generic-optics with 'endPosition' instead"  #-}

-- | The identifier of the policy that was provided as an input.
--
-- /Note:/ Consider using 'sourcePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourcePolicyId :: Lens.Lens' Statement (Core.Maybe Types.SourcePolicyId)
sSourcePolicyId = Lens.field @"sourcePolicyId"
{-# INLINEABLE sSourcePolicyId #-}
{-# DEPRECATED sourcePolicyId "Use generic-lens or generic-optics with 'sourcePolicyId' instead"  #-}

-- | The type of the policy.
--
-- /Note:/ Consider using 'sourcePolicyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourcePolicyType :: Lens.Lens' Statement (Core.Maybe Types.PolicySourceType)
sSourcePolicyType = Lens.field @"sourcePolicyType"
{-# INLINEABLE sSourcePolicyType #-}
{-# DEPRECATED sourcePolicyType "Use generic-lens or generic-optics with 'sourcePolicyType' instead"  #-}

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
--
-- /Note:/ Consider using 'startPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartPosition :: Lens.Lens' Statement (Core.Maybe Types.Position)
sStartPosition = Lens.field @"startPosition"
{-# INLINEABLE sStartPosition #-}
{-# DEPRECATED startPosition "Use generic-lens or generic-optics with 'startPosition' instead"  #-}

instance Core.FromXML Statement where
        parseXML x
          = Statement' Core.<$>
              (x Core..@? "EndPosition") Core.<*> x Core..@? "SourcePolicyId"
                Core.<*> x Core..@? "SourcePolicyType"
                Core.<*> x Core..@? "StartPosition"
