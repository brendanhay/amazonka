{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ResourceError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ResourceError
  ( ResourceError (..),

    -- * Smart constructor
    mkResourceError,

    -- * Lenses
    reErrorCode,
    reErrorMessage,
    reErrorTimestamp,
  )
where

import Network.AWS.AppStream.Types.FleetErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a resource error.
--
-- /See:/ 'mkResourceError' smart constructor.
data ResourceError = ResourceError'
  { -- | The error code.
    errorCode :: Lude.Maybe FleetErrorCode,
    -- | The error message.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The time the error occurred.
    errorTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code.
-- * 'errorMessage' - The error message.
-- * 'errorTimestamp' - The time the error occurred.
mkResourceError ::
  ResourceError
mkResourceError =
  ResourceError'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      errorTimestamp = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorCode :: Lens.Lens' ResourceError (Lude.Maybe FleetErrorCode)
reErrorCode = Lens.lens (errorCode :: ResourceError -> Lude.Maybe FleetErrorCode) (\s a -> s {errorCode = a} :: ResourceError)
{-# DEPRECATED reErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorMessage :: Lens.Lens' ResourceError (Lude.Maybe Lude.Text)
reErrorMessage = Lens.lens (errorMessage :: ResourceError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ResourceError)
{-# DEPRECATED reErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The time the error occurred.
--
-- /Note:/ Consider using 'errorTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorTimestamp :: Lens.Lens' ResourceError (Lude.Maybe Lude.Timestamp)
reErrorTimestamp = Lens.lens (errorTimestamp :: ResourceError -> Lude.Maybe Lude.Timestamp) (\s a -> s {errorTimestamp = a} :: ResourceError)
{-# DEPRECATED reErrorTimestamp "Use generic-lens or generic-optics with 'errorTimestamp' instead." #-}

instance Lude.FromJSON ResourceError where
  parseJSON =
    Lude.withObject
      "ResourceError"
      ( \x ->
          ResourceError'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "ErrorTimestamp")
      )
