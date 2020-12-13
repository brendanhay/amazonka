{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.FailureException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.FailureException
  ( FailureException (..),

    -- * Smart constructor
    mkFailureException,

    -- * Lenses
    feExceptionName,
    feExceptionDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a failure a contributor insights operation.
--
-- /See:/ 'mkFailureException' smart constructor.
data FailureException = FailureException'
  { -- | Exception name.
    exceptionName :: Lude.Maybe Lude.Text,
    -- | Description of the failure.
    exceptionDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureException' with the minimum fields required to make a request.
--
-- * 'exceptionName' - Exception name.
-- * 'exceptionDescription' - Description of the failure.
mkFailureException ::
  FailureException
mkFailureException =
  FailureException'
    { exceptionName = Lude.Nothing,
      exceptionDescription = Lude.Nothing
    }

-- | Exception name.
--
-- /Note:/ Consider using 'exceptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feExceptionName :: Lens.Lens' FailureException (Lude.Maybe Lude.Text)
feExceptionName = Lens.lens (exceptionName :: FailureException -> Lude.Maybe Lude.Text) (\s a -> s {exceptionName = a} :: FailureException)
{-# DEPRECATED feExceptionName "Use generic-lens or generic-optics with 'exceptionName' instead." #-}

-- | Description of the failure.
--
-- /Note:/ Consider using 'exceptionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feExceptionDescription :: Lens.Lens' FailureException (Lude.Maybe Lude.Text)
feExceptionDescription = Lens.lens (exceptionDescription :: FailureException -> Lude.Maybe Lude.Text) (\s a -> s {exceptionDescription = a} :: FailureException)
{-# DEPRECATED feExceptionDescription "Use generic-lens or generic-optics with 'exceptionDescription' instead." #-}

instance Lude.FromJSON FailureException where
  parseJSON =
    Lude.withObject
      "FailureException"
      ( \x ->
          FailureException'
            Lude.<$> (x Lude..:? "ExceptionName")
            Lude.<*> (x Lude..:? "ExceptionDescription")
      )
