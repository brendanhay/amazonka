{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
  ( TaskSucceededEventDetails (..),

    -- * Smart constructor
    mkTaskSucceededEventDetails,

    -- * Lenses
    tsedsOutput,
    tsedsOutputDetails,
    tsedsResourceType,
    tsedsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful completion of a task state.
--
-- /See:/ 'mkTaskSucceededEventDetails' smart constructor.
data TaskSucceededEventDetails = TaskSucceededEventDetails'
  { output ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    outputDetails ::
      Lude.Maybe
        HistoryEventExecutionDataDetails,
    resourceType :: Lude.Text,
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskSucceededEventDetails' with the minimum fields required to make a request.
--
-- * 'output' - The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'outputDetails' - Contains details about the output of an execution history event.
-- * 'resource' - The service name of the resource in a task state.
-- * 'resourceType' - The action of the resource called by a task state.
mkTaskSucceededEventDetails ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resource'
  Lude.Text ->
  TaskSucceededEventDetails
mkTaskSucceededEventDetails pResourceType_ pResource_ =
  TaskSucceededEventDetails'
    { output = Lude.Nothing,
      outputDetails = Lude.Nothing,
      resourceType = pResourceType_,
      resource = pResource_
    }

-- | The full JSON response from a resource when a task has succeeded. This response becomes the output of the related task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsOutput :: Lens.Lens' TaskSucceededEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
tsedsOutput = Lens.lens (output :: TaskSucceededEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {output = a} :: TaskSucceededEventDetails)
{-# DEPRECATED tsedsOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsOutputDetails :: Lens.Lens' TaskSucceededEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
tsedsOutputDetails = Lens.lens (outputDetails :: TaskSucceededEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {outputDetails = a} :: TaskSucceededEventDetails)
{-# DEPRECATED tsedsOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

-- | The action of the resource called by a task state.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsResourceType :: Lens.Lens' TaskSucceededEventDetails Lude.Text
tsedsResourceType = Lens.lens (resourceType :: TaskSucceededEventDetails -> Lude.Text) (\s a -> s {resourceType = a} :: TaskSucceededEventDetails)
{-# DEPRECATED tsedsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The service name of the resource in a task state.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsedsResource :: Lens.Lens' TaskSucceededEventDetails Lude.Text
tsedsResource = Lens.lens (resource :: TaskSucceededEventDetails -> Lude.Text) (\s a -> s {resource = a} :: TaskSucceededEventDetails)
{-# DEPRECATED tsedsResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON TaskSucceededEventDetails where
  parseJSON =
    Lude.withObject
      "TaskSucceededEventDetails"
      ( \x ->
          TaskSucceededEventDetails'
            Lude.<$> (x Lude..:? "output")
            Lude.<*> (x Lude..:? "outputDetails")
            Lude.<*> (x Lude..: "resourceType")
            Lude.<*> (x Lude..: "resource")
      )
