{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationSettings
  ( S3DestinationSettings (..),

    -- * Smart constructor
    mkS3DestinationSettings,

    -- * Lenses
    sdsAccessControl,
    sdsEncryption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.S3DestinationAccessControl as Types
import qualified Network.AWS.MediaConvert.Types.S3EncryptionSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings associated with S3 destination
--
-- /See:/ 'mkS3DestinationSettings' smart constructor.
data S3DestinationSettings = S3DestinationSettings'
  { -- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
    accessControl :: Core.Maybe Types.S3DestinationAccessControl,
    -- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
    encryption :: Core.Maybe Types.S3EncryptionSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DestinationSettings' value with any optional fields omitted.
mkS3DestinationSettings ::
  S3DestinationSettings
mkS3DestinationSettings =
  S3DestinationSettings'
    { accessControl = Core.Nothing,
      encryption = Core.Nothing
    }

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- /Note:/ Consider using 'accessControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsAccessControl :: Lens.Lens' S3DestinationSettings (Core.Maybe Types.S3DestinationAccessControl)
sdsAccessControl = Lens.field @"accessControl"
{-# DEPRECATED sdsAccessControl "Use generic-lens or generic-optics with 'accessControl' instead." #-}

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEncryption :: Lens.Lens' S3DestinationSettings (Core.Maybe Types.S3EncryptionSettings)
sdsEncryption = Lens.field @"encryption"
{-# DEPRECATED sdsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

instance Core.FromJSON S3DestinationSettings where
  toJSON S3DestinationSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("accessControl" Core..=) Core.<$> accessControl,
            ("encryption" Core..=) Core.<$> encryption
          ]
      )

instance Core.FromJSON S3DestinationSettings where
  parseJSON =
    Core.withObject "S3DestinationSettings" Core.$
      \x ->
        S3DestinationSettings'
          Core.<$> (x Core..:? "accessControl") Core.<*> (x Core..:? "encryption")
