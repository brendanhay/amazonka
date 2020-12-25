{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.AppStream.Types.AwsAccountId as Types
import qualified Network.AWS.AppStream.Types.ImagePermissions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the permissions that are available to the specified AWS account for a shared image.
--
-- /See:/ 'mkSharedImagePermissions' smart constructor.
data SharedImagePermissions = SharedImagePermissions'
  { -- | The 12-digit identifier of the AWS account with which the image is shared.
    sharedAccountId :: Types.AwsAccountId,
    -- | Describes the permissions for a shared image.
    imagePermissions :: Types.ImagePermissions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SharedImagePermissions' value with any optional fields omitted.
mkSharedImagePermissions ::
  -- | 'sharedAccountId'
  Types.AwsAccountId ->
  -- | 'imagePermissions'
  Types.ImagePermissions ->
  SharedImagePermissions
mkSharedImagePermissions sharedAccountId imagePermissions =
  SharedImagePermissions' {sharedAccountId, imagePermissions}

-- | The 12-digit identifier of the AWS account with which the image is shared.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipSharedAccountId :: Lens.Lens' SharedImagePermissions Types.AwsAccountId
sipSharedAccountId = Lens.field @"sharedAccountId"
{-# DEPRECATED sipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Describes the permissions for a shared image.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipImagePermissions :: Lens.Lens' SharedImagePermissions Types.ImagePermissions
sipImagePermissions = Lens.field @"imagePermissions"
{-# DEPRECATED sipImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

instance Core.FromJSON SharedImagePermissions where
  parseJSON =
    Core.withObject "SharedImagePermissions" Core.$
      \x ->
        SharedImagePermissions'
          Core.<$> (x Core..: "sharedAccountId")
          Core.<*> (x Core..: "imagePermissions")
