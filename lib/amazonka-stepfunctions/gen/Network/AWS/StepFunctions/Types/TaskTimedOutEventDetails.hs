{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
  ( TaskTimedOutEventDetails (..),

    -- * Smart constructor
    mkTaskTimedOutEventDetails,

    -- * Lenses
    ttoedError,
    ttoedCause,
    ttoedResourceType,
    ttoedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a resource timeout that occurred during an execution.
--
-- /See:/ 'mkTaskTimedOutEventDetails' smart constructor.
data TaskTimedOutEventDetails = TaskTimedOutEventDetails'
  { error ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    cause ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    resourceType :: Lude.Text,
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskTimedOutEventDetails' with the minimum fields required to make a request.
--
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'error' - The error code of the failure.
-- * 'resource' - The service name of the resource in a task state.
-- * 'resourceType' - The action of the resource called by a task state.
mkTaskTimedOutEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskTimedOutEventDetails
mkTaskTimedOutEventDetails pResourceType_ pResource_ =
  TaskTimedOutEventDetails'
    { error = Lude.Nothing,
      cause = Lude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedError :: Lens.Lens' TaskTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
ttoedError = Lens.lens (error :: TaskTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: TaskTimedOutEventDetails)
{-# DEPRECATED ttoedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedCause :: Lens.Lens' TaskTimedOutEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
ttoedCause = Lens.lens (cause :: TaskTimedOutEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: TaskTimedOutEventDetails)
{-# DEPRECATED ttoedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedResourceType :: Lens.Lens' TaskTimedOutEventDetails Lude.Text
ttoedResourceType = Lens.lens (resourceType :: TaskTimedOutEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskTimedOutEventDetails)
{-# DEPRECATED ttoedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttoedResource :: Lens.Lens' TaskTimedOutEventDetails Lude.Text
ttoedResource = Lens.lens (resource :: TaskTimedOutEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskTimedOutEventDetails)
{-# DEPRECATED ttoedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskTimedOutEventDetails where
  parseJSON =
    Lude.withObject
      "TaskTimedOutEventDetails"
      ( \x ->
          TaskTimedOutEventDetails'
            Lude.<$> (x Lude..:? "error")
            Lude.<*> (x Lude..:? "cause")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..: "resource")
      )
