{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.DurationInDays as Types

-- | Contains the configuration settings of a domain.
--
-- /See:/ 'mkDomainConfiguration' smart constructor.
newtype DomainConfiguration = DomainConfiguration'
  { -- | The retention period for workflow executions in this domain.
    workflowExecutionRetentionPeriodInDays :: Types.DurationInDays
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DomainConfiguration' value with any optional fields omitted.
mkDomainConfiguration ::
  -- | 'workflowExecutionRetentionPeriodInDays'
  Types.DurationInDays ->
  DomainConfiguration
mkDomainConfiguration workflowExecutionRetentionPeriodInDays =
  DomainConfiguration' {workflowExecutionRetentionPeriodInDays}

-- | The retention period for workflow executions in this domain.
--
-- /Note:/ Consider using 'workflowExecutionRetentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcWorkflowExecutionRetentionPeriodInDays :: Lens.Lens' DomainConfiguration Types.DurationInDays
dcWorkflowExecutionRetentionPeriodInDays = Lens.field @"workflowExecutionRetentionPeriodInDays"
{-# DEPRECATED dcWorkflowExecutionRetentionPeriodInDays "Use generic-lens or generic-optics with 'workflowExecutionRetentionPeriodInDays' instead." #-}

instance Core.FromJSON DomainConfiguration where
  parseJSON =
    Core.withObject "DomainConfiguration" Core.$
      \x ->
        DomainConfiguration'
          Core.<$> (x Core..: "workflowExecutionRetentionPeriodInDays")
