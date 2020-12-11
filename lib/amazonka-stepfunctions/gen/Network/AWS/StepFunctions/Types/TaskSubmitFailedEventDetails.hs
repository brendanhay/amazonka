-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
  ( TaskSubmitFailedEventDetails (..),

    -- * Smart constructor
    mkTaskSubmitFailedEventDetails,

    -- * Lenses
    tsfedError,
    tsfedCause,
    tsfedResourceType,
    tsfedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a task that failed to submit during an execution.
--
-- /See:/ 'mkTaskSubmitFailedEventDetails' smart constructor.
data TaskSubmitFailedEventDetails = TaskSubmitFailedEventDetails'
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

-- | Creates a value of 'TaskSubmitFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
-- * 'resource' - The service name of the resource in a task state.
-- * 'resourceType' - The action of the resource called by a task state.
mkTaskSubmitFailedEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskSubmitFailedEventDetails
mkTaskSubmitFailedEventDetails pResourceType_ pResource_ =
  TaskSubmitFailedEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedError :: Lens.Lens' TaskSubmitFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tsfedError = Lens.lens (error :: TaskSubmitFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: TaskSubmitFailedEventDetails)
{-# DEPRECATED tsfedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedCause :: Lens.Lens' TaskSubmitFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tsfedCause = Lens.lens (cause :: TaskSubmitFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: TaskSubmitFailedEventDetails)
{-# DEPRECATED tsfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedResourceType :: Lens.Lens' TaskSubmitFailedEventDetails Lude.Text
tsfedResourceType = Lens.lens (resourceType :: TaskSubmitFailedEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskSubmitFailedEventDetails)
{-# DEPRECATED tsfedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfedResource :: Lens.Lens' TaskSubmitFailedEventDetails Lude.Text
tsfedResource = Lens.lens (resource :: TaskSubmitFailedEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskSubmitFailedEventDetails)
{-# DEPRECATED tsfedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskSubmitFailedEventDetails where
  parseJSON =
    Lude.withObject
      "TaskSubmitFailedEventDetails"
      ( \x ->
          TaskSubmitFailedEventDetails'
            Lude.<$> (x Lude..:? "error")
            Lude.<*> (x Lude..:? "cause")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..: "resource")
      )
