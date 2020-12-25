{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DmsTransferSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsTransferSettings
  ( DmsTransferSettings (..),

    -- * Smart constructor
    mkDmsTransferSettings,

    -- * Lenses
    dtsBucketName,
    dtsServiceAccessRoleArn,
  )
where

import qualified Network.AWS.DMS.Types.BucketName as Types
import qualified Network.AWS.DMS.Types.ServiceAccessRoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
-- /See:/ 'mkDmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { -- | The name of the S3 bucket to use.
    bucketName :: Core.Maybe Types.BucketName,
    -- | The IAM role that has permission to access the Amazon S3 bucket.
    serviceAccessRoleArn :: Core.Maybe Types.ServiceAccessRoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DmsTransferSettings' value with any optional fields omitted.
mkDmsTransferSettings ::
  DmsTransferSettings
mkDmsTransferSettings =
  DmsTransferSettings'
    { bucketName = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing
    }

-- | The name of the S3 bucket to use.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsBucketName :: Lens.Lens' DmsTransferSettings (Core.Maybe Types.BucketName)
dtsBucketName = Lens.field @"bucketName"
{-# DEPRECATED dtsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The IAM role that has permission to access the Amazon S3 bucket.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsServiceAccessRoleArn :: Lens.Lens' DmsTransferSettings (Core.Maybe Types.ServiceAccessRoleArn)
dtsServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED dtsServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

instance Core.FromJSON DmsTransferSettings where
  toJSON DmsTransferSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("BucketName" Core..=) Core.<$> bucketName,
            ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn
          ]
      )

instance Core.FromJSON DmsTransferSettings where
  parseJSON =
    Core.withObject "DmsTransferSettings" Core.$
      \x ->
        DmsTransferSettings'
          Core.<$> (x Core..:? "BucketName")
          Core.<*> (x Core..:? "ServiceAccessRoleArn")
