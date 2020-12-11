-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultIdentifier
  ( EvaluationResultIdentifier (..),

    -- * Smart constructor
    mkEvaluationResultIdentifier,

    -- * Lenses
    eriEvaluationResultQualifier,
    eriOrderingTimestamp,
  )
where

import Network.AWS.Config.Types.EvaluationResultQualifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Uniquely identifies an evaluation result.
--
-- /See:/ 'mkEvaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
  { evaluationResultQualifier ::
      Lude.Maybe EvaluationResultQualifier,
    orderingTimestamp ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResultIdentifier' with the minimum fields required to make a request.
--
-- * 'evaluationResultQualifier' - Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
-- * 'orderingTimestamp' - The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
mkEvaluationResultIdentifier ::
  EvaluationResultIdentifier
mkEvaluationResultIdentifier =
  EvaluationResultIdentifier'
    { evaluationResultQualifier =
        Lude.Nothing,
      orderingTimestamp = Lude.Nothing
    }

-- | Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
--
-- /Note:/ Consider using 'evaluationResultQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eriEvaluationResultQualifier :: Lens.Lens' EvaluationResultIdentifier (Lude.Maybe EvaluationResultQualifier)
eriEvaluationResultQualifier = Lens.lens (evaluationResultQualifier :: EvaluationResultIdentifier -> Lude.Maybe EvaluationResultQualifier) (\s a -> s {evaluationResultQualifier = a} :: EvaluationResultIdentifier)
{-# DEPRECATED eriEvaluationResultQualifier "Use generic-lens or generic-optics with 'evaluationResultQualifier' instead." #-}

-- | The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
--
-- /Note:/ Consider using 'orderingTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eriOrderingTimestamp :: Lens.Lens' EvaluationResultIdentifier (Lude.Maybe Lude.Timestamp)
eriOrderingTimestamp = Lens.lens (orderingTimestamp :: EvaluationResultIdentifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {orderingTimestamp = a} :: EvaluationResultIdentifier)
{-# DEPRECATED eriOrderingTimestamp "Use generic-lens or generic-optics with 'orderingTimestamp' instead." #-}

instance Lude.FromJSON EvaluationResultIdentifier where
  parseJSON =
    Lude.withObject
      "EvaluationResultIdentifier"
      ( \x ->
          EvaluationResultIdentifier'
            Lude.<$> (x Lude..:? "EvaluationResultQualifier")
            Lude.<*> (x Lude..:? "OrderingTimestamp")
      )
