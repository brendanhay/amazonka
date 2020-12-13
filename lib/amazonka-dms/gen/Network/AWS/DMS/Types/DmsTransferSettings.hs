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
    dtsServiceAccessRoleARN,
    dtsBucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
-- /See:/ 'mkDmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { -- | The IAM role that has permission to access the Amazon S3 bucket.
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | The name of the S3 bucket to use.
    bucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DmsTransferSettings' with the minimum fields required to make a request.
--
-- * 'serviceAccessRoleARN' - The IAM role that has permission to access the Amazon S3 bucket.
-- * 'bucketName' - The name of the S3 bucket to use.
mkDmsTransferSettings ::
  DmsTransferSettings
mkDmsTransferSettings =
  DmsTransferSettings'
    { serviceAccessRoleARN = Lude.Nothing,
      bucketName = Lude.Nothing
    }

-- | The IAM role that has permission to access the Amazon S3 bucket.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsServiceAccessRoleARN :: Lens.Lens' DmsTransferSettings (Lude.Maybe Lude.Text)
dtsServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: DmsTransferSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: DmsTransferSettings)
{-# DEPRECATED dtsServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | The name of the S3 bucket to use.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsBucketName :: Lens.Lens' DmsTransferSettings (Lude.Maybe Lude.Text)
dtsBucketName = Lens.lens (bucketName :: DmsTransferSettings -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: DmsTransferSettings)
{-# DEPRECATED dtsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

instance Lude.FromJSON DmsTransferSettings where
  parseJSON =
    Lude.withObject
      "DmsTransferSettings"
      ( \x ->
          DmsTransferSettings'
            Lude.<$> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..:? "BucketName")
      )

instance Lude.ToJSON DmsTransferSettings where
  toJSON DmsTransferSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            ("BucketName" Lude..=) Lude.<$> bucketName
          ]
      )
