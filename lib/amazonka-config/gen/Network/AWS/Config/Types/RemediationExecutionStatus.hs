{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStatus
  ( RemediationExecutionStatus (..),

    -- * Smart constructor
    mkRemediationExecutionStatus,

    -- * Lenses
    rState,
    rLastUpdatedTime,
    rResourceKey,
    rStepDetails,
    rInvocationTime,
  )
where

import Network.AWS.Config.Types.RemediationExecutionState
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.ResourceKey
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides details of the current status of the invoked remediation action for that resource.
--
-- /See:/ 'mkRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { state ::
      Lude.Maybe RemediationExecutionState,
    lastUpdatedTime ::
      Lude.Maybe Lude.Timestamp,
    resourceKey :: Lude.Maybe ResourceKey,
    stepDetails ::
      Lude.Maybe [RemediationExecutionStep],
    invocationTime ::
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

-- | Creates a value of 'RemediationExecutionStatus' with the minimum fields required to make a request.
--
-- * 'invocationTime' - Start time when the remediation was executed.
-- * 'lastUpdatedTime' - The time when the remediation execution was last updated.
-- * 'resourceKey' - Undocumented field.
-- * 'state' - ENUM of the values.
-- * 'stepDetails' - Details of every step.
mkRemediationExecutionStatus ::
  RemediationExecutionStatus
mkRemediationExecutionStatus =
  RemediationExecutionStatus'
    { state = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      resourceKey = Lude.Nothing,
      stepDetails = Lude.Nothing,
      invocationTime = Lude.Nothing
    }

-- | ENUM of the values.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' RemediationExecutionStatus (Lude.Maybe RemediationExecutionState)
rState = Lens.lens (state :: RemediationExecutionStatus -> Lude.Maybe RemediationExecutionState) (\s a -> s {state = a} :: RemediationExecutionStatus)
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time when the remediation execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Lude.Maybe Lude.Timestamp)
rLastUpdatedTime = Lens.lens (lastUpdatedTime :: RemediationExecutionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: RemediationExecutionStatus)
{-# DEPRECATED rLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceKey :: Lens.Lens' RemediationExecutionStatus (Lude.Maybe ResourceKey)
rResourceKey = Lens.lens (resourceKey :: RemediationExecutionStatus -> Lude.Maybe ResourceKey) (\s a -> s {resourceKey = a} :: RemediationExecutionStatus)
{-# DEPRECATED rResourceKey "Use generic-lens or generic-optics with 'resourceKey' instead." #-}

-- | Details of every step.
--
-- /Note:/ Consider using 'stepDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStepDetails :: Lens.Lens' RemediationExecutionStatus (Lude.Maybe [RemediationExecutionStep])
rStepDetails = Lens.lens (stepDetails :: RemediationExecutionStatus -> Lude.Maybe [RemediationExecutionStep]) (\s a -> s {stepDetails = a} :: RemediationExecutionStatus)
{-# DEPRECATED rStepDetails "Use generic-lens or generic-optics with 'stepDetails' instead." #-}

-- | Start time when the remediation was executed.
--
-- /Note:/ Consider using 'invocationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInvocationTime :: Lens.Lens' RemediationExecutionStatus (Lude.Maybe Lude.Timestamp)
rInvocationTime = Lens.lens (invocationTime :: RemediationExecutionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {invocationTime = a} :: RemediationExecutionStatus)
{-# DEPRECATED rInvocationTime "Use generic-lens or generic-optics with 'invocationTime' instead." #-}

instance Lude.FromJSON RemediationExecutionStatus where
  parseJSON =
    Lude.withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "LastUpdatedTime")
            Lude.<*> (x Lude..:? "ResourceKey")
            Lude.<*> (x Lude..:? "StepDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InvocationTime")
      )
