{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Statement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Statement
  ( Statement (..),

    -- * Smart constructor
    mkStatement,

    -- * Lenses
    sSourcePolicyType,
    sSourcePolicyId,
    sEndPosition,
    sStartPosition,
  )
where

import Network.AWS.IAM.Types.PolicySourceType
import Network.AWS.IAM.Types.Position
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a reference to a @Statement@ element in a policy document that determines the result of the simulation.
--
-- This data type is used by the @MatchedStatements@ member of the @'EvaluationResult' @ type.
--
-- /See:/ 'mkStatement' smart constructor.
data Statement = Statement'
  { sourcePolicyType ::
      Lude.Maybe PolicySourceType,
    sourcePolicyId :: Lude.Maybe Lude.Text,
    endPosition :: Lude.Maybe Position,
    startPosition :: Lude.Maybe Position
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- * 'endPosition' - The row and column of the end of a @Statement@ in an IAM policy.
-- * 'sourcePolicyId' - The identifier of the policy that was provided as an input.
-- * 'sourcePolicyType' - The type of the policy.
-- * 'startPosition' - The row and column of the beginning of the @Statement@ in an IAM policy.
mkStatement ::
  Statement
mkStatement =
  Statement'
    { sourcePolicyType = Lude.Nothing,
      sourcePolicyId = Lude.Nothing,
      endPosition = Lude.Nothing,
      startPosition = Lude.Nothing
    }

-- | The type of the policy.
--
-- /Note:/ Consider using 'sourcePolicyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourcePolicyType :: Lens.Lens' Statement (Lude.Maybe PolicySourceType)
sSourcePolicyType = Lens.lens (sourcePolicyType :: Statement -> Lude.Maybe PolicySourceType) (\s a -> s {sourcePolicyType = a} :: Statement)
{-# DEPRECATED sSourcePolicyType "Use generic-lens or generic-optics with 'sourcePolicyType' instead." #-}

-- | The identifier of the policy that was provided as an input.
--
-- /Note:/ Consider using 'sourcePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSourcePolicyId :: Lens.Lens' Statement (Lude.Maybe Lude.Text)
sSourcePolicyId = Lens.lens (sourcePolicyId :: Statement -> Lude.Maybe Lude.Text) (\s a -> s {sourcePolicyId = a} :: Statement)
{-# DEPRECATED sSourcePolicyId "Use generic-lens or generic-optics with 'sourcePolicyId' instead." #-}

-- | The row and column of the end of a @Statement@ in an IAM policy.
--
-- /Note:/ Consider using 'endPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndPosition :: Lens.Lens' Statement (Lude.Maybe Position)
sEndPosition = Lens.lens (endPosition :: Statement -> Lude.Maybe Position) (\s a -> s {endPosition = a} :: Statement)
{-# DEPRECATED sEndPosition "Use generic-lens or generic-optics with 'endPosition' instead." #-}

-- | The row and column of the beginning of the @Statement@ in an IAM policy.
--
-- /Note:/ Consider using 'startPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartPosition :: Lens.Lens' Statement (Lude.Maybe Position)
sStartPosition = Lens.lens (startPosition :: Statement -> Lude.Maybe Position) (\s a -> s {startPosition = a} :: Statement)
{-# DEPRECATED sStartPosition "Use generic-lens or generic-optics with 'startPosition' instead." #-}

instance Lude.FromXML Statement where
  parseXML x =
    Statement'
      Lude.<$> (x Lude..@? "SourcePolicyType")
      Lude.<*> (x Lude..@? "SourcePolicyId")
      Lude.<*> (x Lude..@? "EndPosition")
      Lude.<*> (x Lude..@? "StartPosition")
