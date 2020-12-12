{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
  ( TaskStartFailedEventDetails (..),

    -- * Smart constructor
    mkTaskStartFailedEventDetails,

    -- * Lenses
    tsfedsError,
    tsfedsCause,
    tsfedsResourceType,
    tsfedsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a task that failed to start during an execution.
--
-- /See:/ 'mkTaskStartFailedEventDetails' smart constructor.
data TaskStartFailedEventDetails = TaskStartFailedEventDetails'
  { error ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    resourceType :: Lude.Text,
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskStartFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
-- * 'resource' - The service name of the resource in a task state.
-- * 'resourceType' - The action of the resource called by a task state.
mkTaskStartFailedEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskStartFailedEventDetails
mkTaskStartFailedEventDetails pResourceType_ pResource_ =
  TaskStartFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedsError :: Lens.Lens' TaskStartFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tsfedsError = Lens.lens (error :: TaskStartFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: TaskStartFailedEventDetails)
{-# DEPRECATED tsfedsError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedsCause :: Lens.Lens' TaskStartFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tsfedsCause = Lens.lens (cause :: TaskStartFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: TaskStartFailedEventDetails)
{-# DEPRECATED tsfedsCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedsResourceType :: Lens.Lens' TaskStartFailedEventDetails Lude.Text
tsfedsResourceType = Lens.lens (resourceType :: TaskStartFailedEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskStartFailedEventDetails)
{-# DEPRECATED tsfedsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedsResource :: Lens.Lens' TaskStartFailedEventDetails Lude.Text
tsfedsResource = Lens.lens (resource :: TaskStartFailedEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskStartFailedEventDetails)
{-# DEPRECATED tsfedsResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskStartFailedEventDetails where
  parseJSON =
    Lude.withObject
      "TaskStartFailedEventDetails"
      ( \x ->
          TaskStartFailedEventDetails'
            Lude.<$> (x Lude..:? "error")
            Lude.<*> (x Lude..:? "cause")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..: "resource")
      )
