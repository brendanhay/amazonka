-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryS3BucketDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryS3BucketDestination
  ( InventoryS3BucketDestination (..),

    -- * Smart constructor
    mkInventoryS3BucketDestination,

    -- * Lenses
    isbdPrefix,
    isbdAccountId,
    isbdEncryption,
    isbdBucket,
    isbdFormat,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryEncryption
import Network.AWS.S3.Types.InventoryFormat

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
-- /See:/ 'mkInventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { prefix ::
      Lude.Maybe Lude.Text,
    accountId :: Lude.Maybe Lude.Text,
    encryption ::
      Lude.Maybe InventoryEncryption,
    bucket :: BucketName,
    format :: InventoryFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryS3BucketDestination' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
-- * 'bucket' - The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
-- * 'encryption' - Contains the type of server-side encryption used to encrypt the inventory results.
-- * 'format' - Specifies the output format of the inventory results.
-- * 'prefix' - The prefix that is prepended to all inventory results.
mkInventoryS3BucketDestination ::
  -- | 'bucket'
  BucketName ->
  -- | 'format'
  InventoryFormat ->
  InventoryS3BucketDestination
mkInventoryS3BucketDestination pBucket_ pFormat_ =
  InventoryS3BucketDestination'
    { prefix = Lude.Nothing,
      accountId = Lude.Nothing,
      encryption = Lude.Nothing,
      bucket = pBucket_,
      format = pFormat_
    }

-- | The prefix that is prepended to all inventory results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdPrefix :: Lens.Lens' InventoryS3BucketDestination (Lude.Maybe Lude.Text)
isbdPrefix = Lens.lens (prefix :: InventoryS3BucketDestination -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: InventoryS3BucketDestination)
{-# DEPRECATED isbdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdAccountId :: Lens.Lens' InventoryS3BucketDestination (Lude.Maybe Lude.Text)
isbdAccountId = Lens.lens (accountId :: InventoryS3BucketDestination -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: InventoryS3BucketDestination)
{-# DEPRECATED isbdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdEncryption :: Lens.Lens' InventoryS3BucketDestination (Lude.Maybe InventoryEncryption)
isbdEncryption = Lens.lens (encryption :: InventoryS3BucketDestination -> Lude.Maybe InventoryEncryption) (\s a -> s {encryption = a} :: InventoryS3BucketDestination)
{-# DEPRECATED isbdEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdBucket :: Lens.Lens' InventoryS3BucketDestination BucketName
isbdBucket = Lens.lens (bucket :: InventoryS3BucketDestination -> BucketName) (\s a -> s {bucket = a} :: InventoryS3BucketDestination)
{-# DEPRECATED isbdBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the output format of the inventory results.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdFormat :: Lens.Lens' InventoryS3BucketDestination InventoryFormat
isbdFormat = Lens.lens (format :: InventoryS3BucketDestination -> InventoryFormat) (\s a -> s {format = a} :: InventoryS3BucketDestination)
{-# DEPRECATED isbdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.FromXML InventoryS3BucketDestination where
  parseXML x =
    InventoryS3BucketDestination'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "AccountId")
      Lude.<*> (x Lude..@? "Encryption")
      Lude.<*> (x Lude..@ "Bucket")
      Lude.<*> (x Lude..@ "Format")

instance Lude.ToXML InventoryS3BucketDestination where
  toXML InventoryS3BucketDestination' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "AccountId" Lude.@= accountId,
        "Encryption" Lude.@= encryption,
        "Bucket" Lude.@= bucket,
        "Format" Lude.@= format
      ]
