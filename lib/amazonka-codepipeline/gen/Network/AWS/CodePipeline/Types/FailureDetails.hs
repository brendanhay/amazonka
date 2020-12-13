{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureDetails
  ( FailureDetails (..),

    -- * Smart constructor
    mkFailureDetails,

    -- * Lenses
    fdExternalExecutionId,
    fdType,
    fdMessage,
  )
where

import Network.AWS.CodePipeline.Types.FailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about failure details.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The external ID of the run of the action that failed.
    externalExecutionId :: Lude.Maybe Lude.Text,
    -- | The type of the failure.
    type' :: FailureType,
    -- | The message about the failure.
    message :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- * 'externalExecutionId' - The external ID of the run of the action that failed.
-- * 'type'' - The type of the failure.
-- * 'message' - The message about the failure.
mkFailureDetails ::
  -- | 'type''
  FailureType ->
  -- | 'message'
  Lude.Text ->
  FailureDetails
mkFailureDetails pType_ pMessage_ =
  FailureDetails'
    { externalExecutionId = Lude.Nothing,
      type' = pType_,
      message = pMessage_
    }

-- | The external ID of the run of the action that failed.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdExternalExecutionId :: Lens.Lens' FailureDetails (Lude.Maybe Lude.Text)
fdExternalExecutionId = Lens.lens (externalExecutionId :: FailureDetails -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionId = a} :: FailureDetails)
{-# DEPRECATED fdExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The type of the failure.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FailureDetails FailureType
fdType = Lens.lens (type' :: FailureDetails -> FailureType) (\s a -> s {type' = a} :: FailureDetails)
{-# DEPRECATED fdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The message about the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMessage :: Lens.Lens' FailureDetails Lude.Text
fdMessage = Lens.lens (message :: FailureDetails -> Lude.Text) (\s a -> s {message = a} :: FailureDetails)
{-# DEPRECATED fdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.ToJSON FailureDetails where
  toJSON FailureDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("externalExecutionId" Lude..=) Lude.<$> externalExecutionId,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("message" Lude..= message)
          ]
      )
