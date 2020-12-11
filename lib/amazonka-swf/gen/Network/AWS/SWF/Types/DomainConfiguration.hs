-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DomainConfiguration
  ( DomainConfiguration (..),

    -- * Smart constructor
    mkDomainConfiguration,

    -- * Lenses
    dcWorkflowExecutionRetentionPeriodInDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the configuration settings of a domain.
--
-- /See:/ 'mkDomainConfiguration' smart constructor.
newtype DomainConfiguration = DomainConfiguration'
  { workflowExecutionRetentionPeriodInDays ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainConfiguration' with the minimum fields required to make a request.
--
-- * 'workflowExecutionRetentionPeriodInDays' - The retention period for workflow executions in this domain.
mkDomainConfiguration ::
  -- | 'workflowExecutionRetentionPeriodInDays'
  Lude.Text ->
  DomainConfiguration
mkDomainConfiguration pWorkflowExecutionRetentionPeriodInDays_ =
  DomainConfiguration'
    { workflowExecutionRetentionPeriodInDays =
        pWorkflowExecutionRetentionPeriodInDays_
    }

-- | The retention period for workflow executions in this domain.
--
-- /Note:/ Consider using 'workflowExecutionRetentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcWorkflowExecutionRetentionPeriodInDays :: Lens.Lens' DomainConfiguration Lude.Text
dcWorkflowExecutionRetentionPeriodInDays = Lens.lens (workflowExecutionRetentionPeriodInDays :: DomainConfiguration -> Lude.Text) (\s a -> s {workflowExecutionRetentionPeriodInDays = a} :: DomainConfiguration)
{-# DEPRECATED dcWorkflowExecutionRetentionPeriodInDays "Use generic-lens or generic-optics with 'workflowExecutionRetentionPeriodInDays' instead." #-}

instance Lude.FromJSON DomainConfiguration where
  parseJSON =
    Lude.withObject
      "DomainConfiguration"
      ( \x ->
          DomainConfiguration'
            Lude.<$> (x Lude..: "workflowExecutionRetentionPeriodInDays")
      )
