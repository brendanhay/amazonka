{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadException
  ( BatchReadException (..),

    -- * Smart constructor
    mkBatchReadException,

    -- * Lenses
    breType,
    breMessage,
  )
where

import Network.AWS.CloudDirectory.Types.BatchReadExceptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The batch read exception structure, which contains the exception type and message.
--
-- /See:/ 'mkBatchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { -- | A type of exception, such as @InvalidArnException@ .
    type' :: Lude.Maybe BatchReadExceptionType,
    -- | An exception message that is associated with the failure.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchReadException' with the minimum fields required to make a request.
--
-- * 'type'' - A type of exception, such as @InvalidArnException@ .
-- * 'message' - An exception message that is associated with the failure.
mkBatchReadException ::
  BatchReadException
mkBatchReadException =
  BatchReadException' {type' = Lude.Nothing, message = Lude.Nothing}

-- | A type of exception, such as @InvalidArnException@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breType :: Lens.Lens' BatchReadException (Lude.Maybe BatchReadExceptionType)
breType = Lens.lens (type' :: BatchReadException -> Lude.Maybe BatchReadExceptionType) (\s a -> s {type' = a} :: BatchReadException)
{-# DEPRECATED breType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An exception message that is associated with the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breMessage :: Lens.Lens' BatchReadException (Lude.Maybe Lude.Text)
breMessage = Lens.lens (message :: BatchReadException -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: BatchReadException)
{-# DEPRECATED breMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON BatchReadException where
  parseJSON =
    Lude.withObject
      "BatchReadException"
      ( \x ->
          BatchReadException'
            Lude.<$> (x Lude..:? "Type") Lude.<*> (x Lude..:? "Message")
      )
