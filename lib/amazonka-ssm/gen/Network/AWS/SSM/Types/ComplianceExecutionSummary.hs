{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceExecutionSummary
  ( ComplianceExecutionSummary (..),

    -- * Smart constructor
    mkComplianceExecutionSummary,

    -- * Lenses
    cesExecutionId,
    cesExecutionType,
    cesExecutionTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /See:/ 'mkComplianceExecutionSummary' smart constructor.
data ComplianceExecutionSummary = ComplianceExecutionSummary'
  { -- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
    executionId :: Lude.Maybe Lude.Text,
    -- | The type of execution. For example, @Command@ is a valid execution type.
    executionType :: Lude.Maybe Lude.Text,
    -- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
    executionTime :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceExecutionSummary' with the minimum fields required to make a request.
--
-- * 'executionId' - An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
-- * 'executionType' - The type of execution. For example, @Command@ is a valid execution type.
-- * 'executionTime' - The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
mkComplianceExecutionSummary ::
  -- | 'executionTime'
  Lude.Timestamp ->
  ComplianceExecutionSummary
mkComplianceExecutionSummary pExecutionTime_ =
  ComplianceExecutionSummary'
    { executionId = Lude.Nothing,
      executionType = Lude.Nothing,
      executionTime = pExecutionTime_
    }

-- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionId :: Lens.Lens' ComplianceExecutionSummary (Lude.Maybe Lude.Text)
cesExecutionId = Lens.lens (executionId :: ComplianceExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: ComplianceExecutionSummary)
{-# DEPRECATED cesExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The type of execution. For example, @Command@ is a valid execution type.
--
-- /Note:/ Consider using 'executionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionType :: Lens.Lens' ComplianceExecutionSummary (Lude.Maybe Lude.Text)
cesExecutionType = Lens.lens (executionType :: ComplianceExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {executionType = a} :: ComplianceExecutionSummary)
{-# DEPRECATED cesExecutionType "Use generic-lens or generic-optics with 'executionType' instead." #-}

-- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesExecutionTime :: Lens.Lens' ComplianceExecutionSummary Lude.Timestamp
cesExecutionTime = Lens.lens (executionTime :: ComplianceExecutionSummary -> Lude.Timestamp) (\s a -> s {executionTime = a} :: ComplianceExecutionSummary)
{-# DEPRECATED cesExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

instance Lude.FromJSON ComplianceExecutionSummary where
  parseJSON =
    Lude.withObject
      "ComplianceExecutionSummary"
      ( \x ->
          ComplianceExecutionSummary'
            Lude.<$> (x Lude..:? "ExecutionId")
            Lude.<*> (x Lude..:? "ExecutionType")
            Lude.<*> (x Lude..: "ExecutionTime")
      )

instance Lude.ToJSON ComplianceExecutionSummary where
  toJSON ComplianceExecutionSummary' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExecutionId" Lude..=) Lude.<$> executionId,
            ("ExecutionType" Lude..=) Lude.<$> executionType,
            Lude.Just ("ExecutionTime" Lude..= executionTime)
          ]
      )
