-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetError
  ( FleetError (..),

    -- * Smart constructor
    mkFleetError,

    -- * Lenses
    feErrorCode,
    feErrorMessage,
  )
where

import Network.AWS.AppStream.Types.FleetErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a fleet error.
--
-- /See:/ 'mkFleetError' smart constructor.
data FleetError = FleetError'
  { errorCode ::
      Lude.Maybe FleetErrorCode,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code.
-- * 'errorMessage' - The error message.
mkFleetError ::
  FleetError
mkFleetError =
  FleetError'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feErrorCode :: Lens.Lens' FleetError (Lude.Maybe FleetErrorCode)
feErrorCode = Lens.lens (errorCode :: FleetError -> Lude.Maybe FleetErrorCode) (\s a -> s {errorCode = a} :: FleetError)
{-# DEPRECATED feErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feErrorMessage :: Lens.Lens' FleetError (Lude.Maybe Lude.Text)
feErrorMessage = Lens.lens (errorMessage :: FleetError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FleetError)
{-# DEPRECATED feErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON FleetError where
  parseJSON =
    Lude.withObject
      "FleetError"
      ( \x ->
          FleetError'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "ErrorMessage")
      )
