{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationAccessControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationAccessControl
  ( S3DestinationAccessControl (..),

    -- * Smart constructor
    mkS3DestinationAccessControl,

    -- * Lenses
    sdacCannedAcl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.S3ObjectCannedAcl as Types
import qualified Network.AWS.Prelude as Core

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- /See:/ 'mkS3DestinationAccessControl' smart constructor.
newtype S3DestinationAccessControl = S3DestinationAccessControl'
  { -- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
    cannedAcl :: Core.Maybe Types.S3ObjectCannedAcl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationAccessControl' value with any optional fields omitted.
mkS3DestinationAccessControl ::
  S3DestinationAccessControl
mkS3DestinationAccessControl =
  S3DestinationAccessControl' {cannedAcl = Core.Nothing}

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
--
-- /Note:/ Consider using 'cannedAcl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdacCannedAcl :: Lens.Lens' S3DestinationAccessControl (Core.Maybe Types.S3ObjectCannedAcl)
sdacCannedAcl = Lens.field @"cannedAcl"
{-# DEPRECATED sdacCannedAcl "Use generic-lens or generic-optics with 'cannedAcl' instead." #-}

instance Core.FromJSON S3DestinationAccessControl where
  toJSON S3DestinationAccessControl {..} =
    Core.object
      (Core.catMaybes [("cannedAcl" Core..=) Core.<$> cannedAcl])

instance Core.FromJSON S3DestinationAccessControl where
  parseJSON =
    Core.withObject "S3DestinationAccessControl" Core.$
      \x ->
        S3DestinationAccessControl' Core.<$> (x Core..:? "cannedAcl")
