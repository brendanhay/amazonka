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
    wtiStatus,
    wtiWorkflowType,
    wtiDeprecationDate,
    wtiCreationDate,
    wtiDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.RegistrationStatus
import Network.AWS.SWF.Types.WorkflowType

-- | Contains information about a workflow type.
--
-- /See:/ 'mkWorkflowTypeInfo' smart constructor.
data WorkflowTypeInfo = WorkflowTypeInfo'
  { -- | The current status of the workflow type.
    status :: RegistrationStatus,
    -- | The workflow type this information is about.
    workflowType :: WorkflowType,
    -- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
    deprecationDate :: Lude.Maybe Lude.Timestamp,
    -- | The date when this type was registered.
    creationDate :: Lude.Timestamp,
    -- | The description of the type registered through 'RegisterWorkflowType' .
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowTypeInfo' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the workflow type.
-- * 'workflowType' - The workflow type this information is about.
-- * 'deprecationDate' - If the type is in deprecated state, then it is set to the date when the type was deprecated.
-- * 'creationDate' - The date when this type was registered.
-- * 'description' - The description of the type registered through 'RegisterWorkflowType' .
mkWorkflowTypeInfo ::
  -- | 'status'
  RegistrationStatus ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'creationDate'
  Lude.Timestamp ->
  WorkflowTypeInfo
mkWorkflowTypeInfo pStatus_ pWorkflowType_ pCreationDate_ =
  WorkflowTypeInfo'
    { status = pStatus_,
      workflowType = pWorkflowType_,
      deprecationDate = Lude.Nothing,
      creationDate = pCreationDate_,
      description = Lude.Nothing
    }

-- | The current status of the workflow type.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiStatus :: Lens.Lens' WorkflowTypeInfo RegistrationStatus
wtiStatus = Lens.lens (status :: WorkflowTypeInfo -> RegistrationStatus) (\s a -> s {status = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The workflow type this information is about.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiWorkflowType :: Lens.Lens' WorkflowTypeInfo WorkflowType
wtiWorkflowType = Lens.lens (workflowType :: WorkflowTypeInfo -> WorkflowType) (\s a -> s {workflowType = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDeprecationDate :: Lens.Lens' WorkflowTypeInfo (Lude.Maybe Lude.Timestamp)
wtiDeprecationDate = Lens.lens (deprecationDate :: WorkflowTypeInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {deprecationDate = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The date when this type was registered.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiCreationDate :: Lens.Lens' WorkflowTypeInfo Lude.Timestamp
wtiCreationDate = Lens.lens (creationDate :: WorkflowTypeInfo -> Lude.Timestamp) (\s a -> s {creationDate = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The description of the type registered through 'RegisterWorkflowType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDescription :: Lens.Lens' WorkflowTypeInfo (Lude.Maybe Lude.Text)
wtiDescription = Lens.lens (description :: WorkflowTypeInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON WorkflowTypeInfo where
  parseJSON =
    Lude.withObject
      "WorkflowTypeInfo"
      ( \x ->
          WorkflowTypeInfo'
            Lude.<$> (x Lude..: "status")
            Lude.<*> (x Lude..: "workflowType")
            Lude.<*> (x Lude..:? "deprecationDate")
            Lude.<*> (x Lude..: "creationDate")
            Lude.<*> (x Lude..:? "description")
      )
