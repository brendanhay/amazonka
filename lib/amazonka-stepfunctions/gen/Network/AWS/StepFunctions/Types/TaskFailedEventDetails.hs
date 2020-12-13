{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskFailedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskFailedEventDetails
  ( TaskFailedEventDetails (..),

    -- * Smart constructor
    mkTaskFailedEventDetails,

    -- * Lenses
    tfedResourceType,
    tfedError,
    tfedCause,
    tfedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about a task failure event.
--
-- /See:/ 'mkTaskFailedEventDetails' smart constructor.
data TaskFailedEventDetails = TaskFailedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Lude.Text,
    -- | The error code of the failure.
    error :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The service name of the resource in a task state.
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskFailedEventDetails' with the minimum fields required to make a request.
--
-- * 'resourceType' - The action of the resource called by a task state.
-- * 'error' - The error code of the failure.
-- * 'cause' - A more detailed explanation of the cause of the failure.
-- * 'resource' - The service name of the resource in a task state.
mkTaskFailedEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskFailedEventDetails
mkTaskFailedEventDetails pResourceType_ pResource_ =
  TaskFailedEventDetails'
    { resourceType = pResourceType_,
      error = Lude.Nothing,
      cause = Lude.Nothing,
      resource = pResource_
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedResourceType :: Lens.Lens' TaskFailedEventDetails Lude.Text
tfedResourceType = Lens.lens (resourceType :: TaskFailedEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskFailedEventDetails)
{-# DEPRECATED tfedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The error code of the failure.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedError :: Lens.Lens' TaskFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tfedError = Lens.lens (error :: TaskFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {error = a} :: TaskFailedEventDetails)
{-# DEPRECATED tfedError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | A more detailed explanation of the cause of the failure.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedCause :: Lens.Lens' TaskFailedEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tfedCause = Lens.lens (cause :: TaskFailedEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {cause = a} :: TaskFailedEventDetails)
{-# DEPRECATED tfedCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfedResource :: Lens.Lens' TaskFailedEventDetails Lude.Text
tfedResource = Lens.lens (resource :: TaskFailedEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskFailedEventDetails)
{-# DEPRECATED tfedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskFailedEventDetails where
  parseJSON =
    Lude.withObject
      "TaskFailedEventDetails"
      ( \x ->
          TaskFailedEventDetails'
            Lude.<$> (x Lude..: "resourceType")
            Lude.<*> (x Lude..:? "error")
            Lude.<*> (x Lude..:? "cause")
            Lude.<*> (x Lude..: "resource")
      )
