{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailure
  ( ImageFailure (..),

    -- * Smart constructor
    mkImageFailure,

    -- * Lenses
    ifFailureReason,
    ifFailureCode,
    ifImageId,
  )
where

import Network.AWS.ECR.Types.ImageFailureCode
import Network.AWS.ECR.Types.ImageIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an Amazon ECR image failure.
--
-- /See:/ 'mkImageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { failureReason ::
      Lude.Maybe Lude.Text,
    failureCode :: Lude.Maybe ImageFailureCode,
    imageId :: Lude.Maybe ImageIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageFailure' with the minimum fields required to make a request.
--
-- * 'failureCode' - The code associated with the failure.
-- * 'failureReason' - The reason for the failure.
-- * 'imageId' - The image ID associated with the failure.
mkImageFailure ::
  ImageFailure
mkImageFailure =
  ImageFailure'
    { failureReason = Lude.Nothing,
      failureCode = Lude.Nothing,
      imageId = Lude.Nothing
    }

-- | The reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifFailureReason :: Lens.Lens' ImageFailure (Lude.Maybe Lude.Text)
ifFailureReason = Lens.lens (failureReason :: ImageFailure -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: ImageFailure)
{-# DEPRECATED ifFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The code associated with the failure.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifFailureCode :: Lens.Lens' ImageFailure (Lude.Maybe ImageFailureCode)
ifFailureCode = Lens.lens (failureCode :: ImageFailure -> Lude.Maybe ImageFailureCode) (\s a -> s {failureCode = a} :: ImageFailure)
{-# DEPRECATED ifFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The image ID associated with the failure.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageId :: Lens.Lens' ImageFailure (Lude.Maybe ImageIdentifier)
ifImageId = Lens.lens (imageId :: ImageFailure -> Lude.Maybe ImageIdentifier) (\s a -> s {imageId = a} :: ImageFailure)
{-# DEPRECATED ifImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.FromJSON ImageFailure where
  parseJSON =
    Lude.withObject
      "ImageFailure"
      ( \x ->
          ImageFailure'
            Lude.<$> (x Lude..:? "failureReason")
            Lude.<*> (x Lude..:? "failureCode")
            Lude.<*> (x Lude..:? "imageId")
      )
