{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeInfo
  ( WorkflowTypeInfo (..),

    -- * Smart constructor
    mkWorkflowTypeInfo,

    -- * Lenses
    wtiWorkflowType,
    wtiStatus,
    wtiCreationDate,
    wtiDeprecationDate,
    wtiDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Description as Types
import qualified Network.AWS.SWF.Types.RegistrationStatus as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Contains information about a workflow type.
--
-- /See:/ 'mkWorkflowTypeInfo' smart constructor.
data WorkflowTypeInfo = WorkflowTypeInfo'
  { -- | The workflow type this information is about.
    workflowType :: Types.WorkflowType,
    -- | The current status of the workflow type.
    status :: Types.RegistrationStatus,
    -- | The date when this type was registered.
    creationDate :: Core.NominalDiffTime,
    -- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
    deprecationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the type registered through 'RegisterWorkflowType' .
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'WorkflowTypeInfo' value with any optional fields omitted.
mkWorkflowTypeInfo ::
  -- | 'workflowType'
  Types.WorkflowType ->
  -- | 'status'
  Types.RegistrationStatus ->
  -- | 'creationDate'
  Core.NominalDiffTime ->
  WorkflowTypeInfo
mkWorkflowTypeInfo workflowType status creationDate =
  WorkflowTypeInfo'
    { workflowType,
      status,
      creationDate,
      deprecationDate = Core.Nothing,
      description = Core.Nothing
    }

-- | The workflow type this information is about.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiWorkflowType :: Lens.Lens' WorkflowTypeInfo Types.WorkflowType
wtiWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED wtiWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The current status of the workflow type.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiStatus :: Lens.Lens' WorkflowTypeInfo Types.RegistrationStatus
wtiStatus = Lens.field @"status"
{-# DEPRECATED wtiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when this type was registered.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiCreationDate :: Lens.Lens' WorkflowTypeInfo Core.NominalDiffTime
wtiCreationDate = Lens.field @"creationDate"
{-# DEPRECATED wtiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDeprecationDate :: Lens.Lens' WorkflowTypeInfo (Core.Maybe Core.NominalDiffTime)
wtiDeprecationDate = Lens.field @"deprecationDate"
{-# DEPRECATED wtiDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The description of the type registered through 'RegisterWorkflowType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDescription :: Lens.Lens' WorkflowTypeInfo (Core.Maybe Types.Description)
wtiDescription = Lens.field @"description"
{-# DEPRECATED wtiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON WorkflowTypeInfo where
  parseJSON =
    Core.withObject "WorkflowTypeInfo" Core.$
      \x ->
        WorkflowTypeInfo'
          Core.<$> (x Core..: "workflowType")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..: "creationDate")
          Core.<*> (x Core..:? "deprecationDate")
          Core.<*> (x Core..:? "description")
