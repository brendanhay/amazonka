{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLCandidateStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLCandidateStep
  ( AutoMLCandidateStep (..),

    -- * Smart constructor
    mkAutoMLCandidateStep,

    -- * Lenses
    amlcsCandidateStepType,
    amlcsCandidateStepARN,
    amlcsCandidateStepName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CandidateStepType

-- | Information about the steps for a Candidate, and what step it is working on.
--
-- /See:/ 'mkAutoMLCandidateStep' smart constructor.
data AutoMLCandidateStep = AutoMLCandidateStep'
  { candidateStepType ::
      CandidateStepType,
    candidateStepARN :: Lude.Text,
    candidateStepName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLCandidateStep' with the minimum fields required to make a request.
--
-- * 'candidateStepARN' - The ARN for the Candidate's step.
-- * 'candidateStepName' - The name for the Candidate's step.
-- * 'candidateStepType' - Whether the Candidate is at the transform, training, or processing step.
mkAutoMLCandidateStep ::
  -- | 'candidateStepType'
  CandidateStepType ->
  -- | 'candidateStepARN'
  Lude.Text ->
  -- | 'candidateStepName'
  Lude.Text ->
  AutoMLCandidateStep
mkAutoMLCandidateStep
  pCandidateStepType_
  pCandidateStepARN_
  pCandidateStepName_ =
    AutoMLCandidateStep'
      { candidateStepType = pCandidateStepType_,
        candidateStepARN = pCandidateStepARN_,
        candidateStepName = pCandidateStepName_
      }

-- | Whether the Candidate is at the transform, training, or processing step.
--
-- /Note:/ Consider using 'candidateStepType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepType :: Lens.Lens' AutoMLCandidateStep CandidateStepType
amlcsCandidateStepType = Lens.lens (candidateStepType :: AutoMLCandidateStep -> CandidateStepType) (\s a -> s {candidateStepType = a} :: AutoMLCandidateStep)
{-# DEPRECATED amlcsCandidateStepType "Use generic-lens or generic-optics with 'candidateStepType' instead." #-}

-- | The ARN for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepARN :: Lens.Lens' AutoMLCandidateStep Lude.Text
amlcsCandidateStepARN = Lens.lens (candidateStepARN :: AutoMLCandidateStep -> Lude.Text) (\s a -> s {candidateStepARN = a} :: AutoMLCandidateStep)
{-# DEPRECATED amlcsCandidateStepARN "Use generic-lens or generic-optics with 'candidateStepARN' instead." #-}

-- | The name for the Candidate's step.
--
-- /Note:/ Consider using 'candidateStepName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcsCandidateStepName :: Lens.Lens' AutoMLCandidateStep Lude.Text
amlcsCandidateStepName = Lens.lens (candidateStepName :: AutoMLCandidateStep -> Lude.Text) (\s a -> s {candidateStepName = a} :: AutoMLCandidateStep)
{-# DEPRECATED amlcsCandidateStepName "Use generic-lens or generic-optics with 'candidateStepName' instead." #-}

instance Lude.FromJSON AutoMLCandidateStep where
  parseJSON =
    Lude.withObject
      "AutoMLCandidateStep"
      ( \x ->
          AutoMLCandidateStep'
            Lude.<$> (x Lude..: "CandidateStepType")
            Lude.<*> (x Lude..: "CandidateStepArn")
            Lude.<*> (x Lude..: "CandidateStepName")
      )
