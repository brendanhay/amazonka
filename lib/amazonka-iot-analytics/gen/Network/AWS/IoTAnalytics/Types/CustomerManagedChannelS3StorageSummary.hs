{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
  ( CustomerManagedChannelS3StorageSummary (..),

    -- * Smart constructor
    mkCustomerManagedChannelS3StorageSummary,

    -- * Lenses
    cmcsssBucket,
    cmcsssKeyPrefix,
    cmcsssRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /See:/ 'mkCustomerManagedChannelS3StorageSummary' smart constructor.
data CustomerManagedChannelS3StorageSummary = CustomerManagedChannelS3StorageSummary'
  { bucket ::
      Lude.Maybe
        Lude.Text,
    keyPrefix ::
      Lude.Maybe
        Lude.Text,
    roleARN ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomerManagedChannelS3StorageSummary' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket in which channel data is stored.
-- * 'keyPrefix' - Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
mkCustomerManagedChannelS3StorageSummary ::
  CustomerManagedChannelS3StorageSummary
mkCustomerManagedChannelS3StorageSummary =
  CustomerManagedChannelS3StorageSummary'
    { bucket = Lude.Nothing,
      keyPrefix = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The name of the S3 bucket in which channel data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssBucket :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Lude.Maybe Lude.Text)
cmcsssBucket = Lens.lens (bucket :: CustomerManagedChannelS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: CustomerManagedChannelS3StorageSummary)
{-# DEPRECATED cmcsssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssKeyPrefix :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Lude.Maybe Lude.Text)
cmcsssKeyPrefix = Lens.lens (keyPrefix :: CustomerManagedChannelS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {keyPrefix = a} :: CustomerManagedChannelS3StorageSummary)
{-# DEPRECATED cmcsssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssRoleARN :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Lude.Maybe Lude.Text)
cmcsssRoleARN = Lens.lens (roleARN :: CustomerManagedChannelS3StorageSummary -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CustomerManagedChannelS3StorageSummary)
{-# DEPRECATED cmcsssRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CustomerManagedChannelS3StorageSummary where
  parseJSON =
    Lude.withObject
      "CustomerManagedChannelS3StorageSummary"
      ( \x ->
          CustomerManagedChannelS3StorageSummary'
            Lude.<$> (x Lude..:? "bucket")
            Lude.<*> (x Lude..:? "keyPrefix")
            Lude.<*> (x Lude..:? "roleArn")
      )
