{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskStartedEventDetails
  ( TaskStartedEventDetails (..),

    -- * Smart constructor
    mkTaskStartedEventDetails,

    -- * Lenses
    tsedResourceType,
    tsedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the start of a task during an execution.
--
-- /See:/ 'mkTaskStartedEventDetails' smart constructor.
data TaskStartedEventDetails = TaskStartedEventDetails'
  { -- | The action of the resource called by a task state.
    resourceType :: Lude.Text,
    -- | The service name of the resource in a task state.
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskStartedEventDetails' with the minimum fields required to make a request.
--
-- * 'resourceType' - The action of the resource called by a task state.
-- * 'resource' - The service name of the resource in a task state.
mkTaskStartedEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskStartedEventDetails
mkTaskStartedEventDetails pResourceType_ pResource_ =
  TaskStartedEventDetails'
    { resourceType = pResourceType_,
      resource = pResource_
    }

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedResourceType :: Lens.Lens' TaskStartedEventDetails Lude.Text
tsedResourceType = Lens.lens (resourceType :: TaskStartedEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskStartedEventDetails)
{-# DEPRECATED tsedResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedResource :: Lens.Lens' TaskStartedEventDetails Lude.Text
tsedResource = Lens.lens (resource :: TaskStartedEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskStartedEventDetails)
{-# DEPRECATED tsedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskStartedEventDetails where
  parseJSON =
    Lude.withObject
      "TaskStartedEventDetails"
      ( \x ->
          TaskStartedEventDetails'
            Lude.<$> (x Lude..: "resourceType") Lude.<*> (x Lude..: "resource")
      )
