{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
  ( CustomerManagedDatastoreS3StorageSummary (..),

    -- * Smart constructor
    mkCustomerManagedDatastoreS3StorageSummary,

    -- * Lenses
    cmdsssBucket,
    cmdsssKeyPrefix,
    cmdsssRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to store data store data in an S3 bucket that you manage.
--
-- /See:/ 'mkCustomerManagedDatastoreS3StorageSummary' smart constructor.
data CustomerManagedDatastoreS3StorageSummary = CustomerManagedDatastoreS3StorageSummary'
  { -- | The name of the S3 bucket in which data store data is stored.
    bucket :: Lude.Maybe Lude.Text,
    -- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
    keyPrefix :: Lude.Maybe Lude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerManagedDatastoreS3StorageSummary' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket in which data store data is stored.
-- * 'keyPrefix' - Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
mkCustomerManagedDatastoreS3StorageSummary ::
  CustomerManagedDatastoreS3StorageSummary
mkCustomerManagedDatastoreS3StorageSummary =
  CustomerManagedDatastoreS3StorageSummary'
    { bucket = Lude.Nothing,
      keyPrefix = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The name of the S3 bucket in which data store data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssBucket :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Lude.Maybe Lude.Text)
cmdsssBucket = Lens.lens (bucket :: CustomerManagedDatastoreS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: CustomerManagedDatastoreS3StorageSummary)
{-# DEPRECATED cmdsssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssKeyPrefix :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Lude.Maybe Lude.Text)
cmdsssKeyPrefix = Lens.lens (keyPrefix :: CustomerManagedDatastoreS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {keyPrefix = a} :: CustomerManagedDatastoreS3StorageSummary)
{-# DEPRECATED cmdsssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssRoleARN :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Lude.Maybe Lude.Text)
cmdsssRoleARN = Lens.lens (roleARN :: CustomerManagedDatastoreS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CustomerManagedDatastoreS3StorageSummary)
{-# DEPRECATED cmdsssRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CustomerManagedDatastoreS3StorageSummary where
  parseJSON =
    Lude.withObject
      "CustomerManagedDatastoreS3StorageSummary"
      ( \x ->
          CustomerManagedDatastoreS3StorageSummary'
            Lude.<$> (x Lude..:? "bucket")
            Lude.<*> (x Lude..:? "keyPrefix")
            Lude.<*> (x Lude..:? "roleArn")
      )
