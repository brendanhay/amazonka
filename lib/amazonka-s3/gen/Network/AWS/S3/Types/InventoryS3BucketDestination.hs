{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    isbdBucket,
    isbdFormat,
    isbdAccountId,
    isbdEncryption,
    isbdPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AccountId as Types
import qualified Network.AWS.S3.Types.InventoryEncryption as Types
import qualified Network.AWS.S3.Types.InventoryFormat as Types
import qualified Network.AWS.S3.Types.Prefix as Types

-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
--
-- /See:/ 'mkInventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { -- | The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
    bucket :: Types.BucketName,
    -- | Specifies the output format of the inventory results.
    format :: Types.InventoryFormat,
    -- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
    accountId :: Core.Maybe Types.AccountId,
    -- | Contains the type of server-side encryption used to encrypt the inventory results.
    encryption :: Core.Maybe Types.InventoryEncryption,
    -- | The prefix that is prepended to all inventory results.
    prefix :: Core.Maybe Types.Prefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryS3BucketDestination' value with any optional fields omitted.
mkInventoryS3BucketDestination ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'format'
  Types.InventoryFormat ->
  InventoryS3BucketDestination
mkInventoryS3BucketDestination bucket format =
  InventoryS3BucketDestination'
    { bucket,
      format,
      accountId = Core.Nothing,
      encryption = Core.Nothing,
      prefix = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the bucket where inventory results will be published.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdBucket :: Lens.Lens' InventoryS3BucketDestination Types.BucketName
isbdBucket = Lens.field @"bucket"
{-# DEPRECATED isbdBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the output format of the inventory results.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdFormat :: Lens.Lens' InventoryS3BucketDestination Types.InventoryFormat
isbdFormat = Lens.field @"format"
{-# DEPRECATED isbdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdAccountId :: Lens.Lens' InventoryS3BucketDestination (Core.Maybe Types.AccountId)
isbdAccountId = Lens.field @"accountId"
{-# DEPRECATED isbdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdEncryption :: Lens.Lens' InventoryS3BucketDestination (Core.Maybe Types.InventoryEncryption)
isbdEncryption = Lens.field @"encryption"
{-# DEPRECATED isbdEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The prefix that is prepended to all inventory results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isbdPrefix :: Lens.Lens' InventoryS3BucketDestination (Core.Maybe Types.Prefix)
isbdPrefix = Lens.field @"prefix"
{-# DEPRECATED isbdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Core.ToXML InventoryS3BucketDestination where
  toXML InventoryS3BucketDestination {..} =
    Core.toXMLNode "Bucket" bucket
      Core.<> Core.toXMLNode "Format" format
      Core.<> Core.toXMLNode "AccountId" Core.<$> accountId
      Core.<> Core.toXMLNode "Encryption" Core.<$> encryption
      Core.<> Core.toXMLNode "Prefix" Core.<$> prefix

instance Core.FromXML InventoryS3BucketDestination where
  parseXML x =
    InventoryS3BucketDestination'
      Core.<$> (x Core..@ "Bucket")
      Core.<*> (x Core..@ "Format")
      Core.<*> (x Core..@? "AccountId")
      Core.<*> (x Core..@? "Encryption")
      Core.<*> (x Core..@? "Prefix")
