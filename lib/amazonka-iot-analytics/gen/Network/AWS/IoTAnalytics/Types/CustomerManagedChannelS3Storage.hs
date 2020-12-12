{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
  ( CustomerManagedChannelS3Storage (..),

    -- * Smart constructor
    mkCustomerManagedChannelS3Storage,

    -- * Lenses
    cmcssKeyPrefix,
    cmcssBucket,
    cmcssRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /See:/ 'mkCustomerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'
  { keyPrefix ::
      Lude.Maybe Lude.Text,
    bucket :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerManagedChannelS3Storage' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket in which channel data is stored.
-- * 'keyPrefix' - Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
mkCustomerManagedChannelS3Storage ::
  -- | 'bucket'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CustomerManagedChannelS3Storage
mkCustomerManagedChannelS3Storage pBucket_ pRoleARN_ =
  CustomerManagedChannelS3Storage'
    { keyPrefix = Lude.Nothing,
      bucket = pBucket_,
      roleARN = pRoleARN_
    }

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssKeyPrefix :: Lens.Lens' CustomerManagedChannelS3Storage (Lude.Maybe Lude.Text)
cmcssKeyPrefix = Lens.lens (keyPrefix :: CustomerManagedChannelS3Storage -> Lude.Maybe Lude.Text) (\s a -> s {keyPrefix = a} :: CustomerManagedChannelS3Storage)
{-# DEPRECATED cmcssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

-- | The name of the S3 bucket in which channel data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssBucket :: Lens.Lens' CustomerManagedChannelS3Storage Lude.Text
cmcssBucket = Lens.lens (bucket :: CustomerManagedChannelS3Storage -> Lude.Text) (\s a -> s {bucket = a} :: CustomerManagedChannelS3Storage)
{-# DEPRECATED cmcssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssRoleARN :: Lens.Lens' CustomerManagedChannelS3Storage Lude.Text
cmcssRoleARN = Lens.lens (roleARN :: CustomerManagedChannelS3Storage -> Lude.Text) (\s a -> s {roleARN = a} :: CustomerManagedChannelS3Storage)
{-# DEPRECATED cmcssRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CustomerManagedChannelS3Storage where
  parseJSON =
    Lude.withObject
      "CustomerManagedChannelS3Storage"
      ( \x ->
          CustomerManagedChannelS3Storage'
            Lude.<$> (x Lude..:? "keyPrefix")
            Lude.<*> (x Lude..: "bucket")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON CustomerManagedChannelS3Storage where
  toJSON CustomerManagedChannelS3Storage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyPrefix" Lude..=) Lude.<$> keyPrefix,
            Lude.Just ("bucket" Lude..= bucket),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
