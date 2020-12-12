{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
  ( CustomerManagedDatastoreS3Storage (..),

    -- * Smart constructor
    mkCustomerManagedDatastoreS3Storage,

    -- * Lenses
    cmdssKeyPrefix,
    cmdssBucket,
    cmdssRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this to store data store data in an S3 bucket that you manage. When customer-managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /See:/ 'mkCustomerManagedDatastoreS3Storage' smart constructor.
data CustomerManagedDatastoreS3Storage = CustomerManagedDatastoreS3Storage'
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

-- | Creates a value of 'CustomerManagedDatastoreS3Storage' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket in which data store data is stored.
-- * 'keyPrefix' - Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
mkCustomerManagedDatastoreS3Storage ::
  -- | 'bucket'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CustomerManagedDatastoreS3Storage
mkCustomerManagedDatastoreS3Storage pBucket_ pRoleARN_ =
  CustomerManagedDatastoreS3Storage'
    { keyPrefix = Lude.Nothing,
      bucket = pBucket_,
      roleARN = pRoleARN_
    }

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssKeyPrefix :: Lens.Lens' CustomerManagedDatastoreS3Storage (Lude.Maybe Lude.Text)
cmdssKeyPrefix = Lens.lens (keyPrefix :: CustomerManagedDatastoreS3Storage -> Lude.Maybe Lude.Text) (\s a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3Storage)
{-# DEPRECATED cmdssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

-- | The name of the S3 bucket in which data store data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssBucket :: Lens.Lens' CustomerManagedDatastoreS3Storage Lude.Text
cmdssBucket = Lens.lens (bucket :: CustomerManagedDatastoreS3Storage -> Lude.Text) (\s a -> s {bucket = a} :: CustomerManagedDatastoreS3Storage)
{-# DEPRECATED cmdssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssRoleARN :: Lens.Lens' CustomerManagedDatastoreS3Storage Lude.Text
cmdssRoleARN = Lens.lens (roleARN :: CustomerManagedDatastoreS3Storage -> Lude.Text) (\s a -> s {roleARN = a} :: CustomerManagedDatastoreS3Storage)
{-# DEPRECATED cmdssRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CustomerManagedDatastoreS3Storage where
  parseJSON =
    Lude.withObject
      "CustomerManagedDatastoreS3Storage"
      ( \x ->
          CustomerManagedDatastoreS3Storage'
            Lude.<$> (x Lude..:? "keyPrefix")
            Lude.<*> (x Lude..: "bucket")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON CustomerManagedDatastoreS3Storage where
  toJSON CustomerManagedDatastoreS3Storage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyPrefix" Lude..=) Lude.<$> keyPrefix,
            Lude.Just ("bucket" Lude..= bucket),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
