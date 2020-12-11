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
    wtiDeprecationDate,
    wtiDescription,
    wtiWorkflowType,
    wtiStatus,
    wtiCreationDate,
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
  { deprecationDate ::
      Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    workflowType :: WorkflowType,
    status :: RegistrationStatus,
    creationDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowTypeInfo' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date when this type was registered.
-- * 'deprecationDate' - If the type is in deprecated state, then it is set to the date when the type was deprecated.
-- * 'description' - The description of the type registered through 'RegisterWorkflowType' .
-- * 'status' - The current status of the workflow type.
-- * 'workflowType' - The workflow type this information is about.
mkWorkflowTypeInfo ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'status'
  RegistrationStatus ->
  -- | 'creationDate'
  Lude.Timestamp ->
  WorkflowTypeInfo
mkWorkflowTypeInfo pWorkflowType_ pStatus_ pCreationDate_ =
  WorkflowTypeInfo'
    { deprecationDate = Lude.Nothing,
      description = Lude.Nothing,
      workflowType = pWorkflowType_,
      status = pStatus_,
      creationDate = pCreationDate_
    }

-- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDeprecationDate :: Lens.Lens' WorkflowTypeInfo (Lude.Maybe Lude.Timestamp)
wtiDeprecationDate = Lens.lens (deprecationDate :: WorkflowTypeInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {deprecationDate = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The description of the type registered through 'RegisterWorkflowType' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiDescription :: Lens.Lens' WorkflowTypeInfo (Lude.Maybe Lude.Text)
wtiDescription = Lens.lens (description :: WorkflowTypeInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The workflow type this information is about.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiWorkflowType :: Lens.Lens' WorkflowTypeInfo WorkflowType
wtiWorkflowType = Lens.lens (workflowType :: WorkflowTypeInfo -> WorkflowType) (\s a -> s {workflowType = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The current status of the workflow type.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiStatus :: Lens.Lens' WorkflowTypeInfo RegistrationStatus
wtiStatus = Lens.lens (status :: WorkflowTypeInfo -> RegistrationStatus) (\s a -> s {status = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when this type was registered.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtiCreationDate :: Lens.Lens' WorkflowTypeInfo Lude.Timestamp
wtiCreationDate = Lens.lens (creationDate :: WorkflowTypeInfo -> Lude.Timestamp) (\s a -> s {creationDate = a} :: WorkflowTypeInfo)
{-# DEPRECATED wtiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON WorkflowTypeInfo where
  parseJSON =
    Lude.withObject
      "WorkflowTypeInfo"
      ( \x ->
          WorkflowTypeInfo'
            Lude.<$> (x Lude..:? "deprecationDate")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "workflowType")
            Lude.<*> (x Lude..: "status")
            Lude.<*> (x Lude..: "creationDate")
      )
