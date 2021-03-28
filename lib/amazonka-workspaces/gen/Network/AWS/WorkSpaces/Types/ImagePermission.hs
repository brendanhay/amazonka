{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ImagePermission
  ( ImagePermission (..)
  -- * Smart constructor
  , mkImagePermission
  -- * Lenses
  , ipSharedAccountId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AwsAccount as Types

-- | Describes the AWS accounts that have been granted permission to use a shared image. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
--
-- /See:/ 'mkImagePermission' smart constructor.
newtype ImagePermission = ImagePermission'
  { sharedAccountId :: Core.Maybe Types.AwsAccount
    -- ^ The identifier of the AWS account that an image has been shared with.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImagePermission' value with any optional fields omitted.
mkImagePermission
    :: ImagePermission
mkImagePermission
  = ImagePermission'{sharedAccountId = Core.Nothing}

-- | The identifier of the AWS account that an image has been shared with.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipSharedAccountId :: Lens.Lens' ImagePermission (Core.Maybe Types.AwsAccount)
ipSharedAccountId = Lens.field @"sharedAccountId"
{-# INLINEABLE ipSharedAccountId #-}
{-# DEPRECATED sharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead"  #-}

instance Core.FromJSON ImagePermission where
        parseJSON
          = Core.withObject "ImagePermission" Core.$
              \ x -> ImagePermission' Core.<$> (x Core..:? "SharedAccountId")
