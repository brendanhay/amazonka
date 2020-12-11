-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.FailedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.FailedResource
  ( FailedResource (..),

    -- * Smart constructor
    mkFailedResource,

    -- * Lenses
    frResourceARN,
    frErrorCode,
    frErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A resource that failed to be added to or removed from a group.
--
-- /See:/ 'mkFailedResource' smart constructor.
data FailedResource = FailedResource'
  { resourceARN ::
      Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'FailedResource' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code associated with the failure.
-- * 'errorMessage' - The error message text associated with the failure.
-- * 'resourceARN' - The ARN of the resource that failed to be added or removed.
mkFailedResource ::
  FailedResource
mkFailedResource =
  FailedResource'
    { resourceARN = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The ARN of the resource that failed to be added or removed.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frResourceARN :: Lens.Lens' FailedResource (Lude.Maybe Lude.Text)
frResourceARN = Lens.lens (resourceARN :: FailedResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: FailedResource)
{-# DEPRECATED frResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The error code associated with the failure.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frErrorCode :: Lens.Lens' FailedResource (Lude.Maybe Lude.Text)
frErrorCode = Lens.lens (errorCode :: FailedResource -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: FailedResource)
{-# DEPRECATED frErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message text associated with the failure.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frErrorMessage :: Lens.Lens' FailedResource (Lude.Maybe Lude.Text)
frErrorMessage = Lens.lens (errorMessage :: FailedResource -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailedResource)
{-# DEPRECATED frErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON FailedResource where
  parseJSON =
    Lude.withObject
      "FailedResource"
      ( \x ->
          FailedResource'
            Lude.<$> (x Lude..:? "ResourceArn")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
