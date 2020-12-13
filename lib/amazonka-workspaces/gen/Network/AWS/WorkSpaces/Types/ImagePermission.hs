{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ImagePermission
  ( ImagePermission (..),

    -- * Smart constructor
    mkImagePermission,

    -- * Lenses
    ipSharedAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the AWS accounts that have been granted permission to use a shared image. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
--
-- /See:/ 'mkImagePermission' smart constructor.
newtype ImagePermission = ImagePermission'
  { -- | The identifier of the AWS account that an image has been shared with.
    sharedAccountId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImagePermission' with the minimum fields required to make a request.
--
-- * 'sharedAccountId' - The identifier of the AWS account that an image has been shared with.
mkImagePermission ::
  ImagePermission
mkImagePermission =
  ImagePermission' {sharedAccountId = Lude.Nothing}

-- | The identifier of the AWS account that an image has been shared with.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipSharedAccountId :: Lens.Lens' ImagePermission (Lude.Maybe Lude.Text)
ipSharedAccountId = Lens.lens (sharedAccountId :: ImagePermission -> Lude.Maybe Lude.Text) (\s a -> s {sharedAccountId = a} :: ImagePermission)
{-# DEPRECATED ipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

instance Lude.FromJSON ImagePermission where
  parseJSON =
    Lude.withObject
      "ImagePermission"
      (\x -> ImagePermission' Lude.<$> (x Lude..:? "SharedAccountId"))
