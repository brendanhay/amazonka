-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.SharedImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SharedImagePermissions
  ( SharedImagePermissions (..),

    -- * Smart constructor
    mkSharedImagePermissions,

    -- * Lenses
    sipSharedAccountId,
    sipImagePermissions,
  )
where

import Network.AWS.AppStream.Types.ImagePermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the permissions that are available to the specified AWS account for a shared image.
--
-- /See:/ 'mkSharedImagePermissions' smart constructor.
data SharedImagePermissions = SharedImagePermissions'
  { sharedAccountId ::
      Lude.Text,
    imagePermissions :: ImagePermissions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SharedImagePermissions' with the minimum fields required to make a request.
--
-- * 'imagePermissions' - Describes the permissions for a shared image.
-- * 'sharedAccountId' - The 12-digit identifier of the AWS account with which the image is shared.
mkSharedImagePermissions ::
  -- | 'sharedAccountId'
  Lude.Text ->
  -- | 'imagePermissions'
  ImagePermissions ->
  SharedImagePermissions
mkSharedImagePermissions pSharedAccountId_ pImagePermissions_ =
  SharedImagePermissions'
    { sharedAccountId = pSharedAccountId_,
      imagePermissions = pImagePermissions_
    }

-- | The 12-digit identifier of the AWS account with which the image is shared.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipSharedAccountId :: Lens.Lens' SharedImagePermissions Lude.Text
sipSharedAccountId = Lens.lens (sharedAccountId :: SharedImagePermissions -> Lude.Text) (\s a -> s {sharedAccountId = a} :: SharedImagePermissions)
{-# DEPRECATED sipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Describes the permissions for a shared image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipImagePermissions :: Lens.Lens' SharedImagePermissions ImagePermissions
sipImagePermissions = Lens.lens (imagePermissions :: SharedImagePermissions -> ImagePermissions) (\s a -> s {imagePermissions = a} :: SharedImagePermissions)
{-# DEPRECATED sipImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

instance Lude.FromJSON SharedImagePermissions where
  parseJSON =
    Lude.withObject
      "SharedImagePermissions"
      ( \x ->
          SharedImagePermissions'
            Lude.<$> (x Lude..: "sharedAccountId")
            Lude.<*> (x Lude..: "imagePermissions")
      )
