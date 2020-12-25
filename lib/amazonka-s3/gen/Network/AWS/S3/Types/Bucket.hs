{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Bucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Bucket
  ( Bucket (..),

    -- * Smart constructor
    mkBucket,

    -- * Lenses
    bCreationDate,
    bName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Name as Types

-- | In terms of implementation, a Bucket is a resource. An Amazon S3 bucket name is globally unique, and the namespace is shared by all AWS accounts.
--
-- /See:/ 'mkBucket' smart constructor.
data Bucket = Bucket'
  { -- | Date the bucket was created.
    creationDate :: Core.UTCTime,
    -- | The name of the bucket.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Bucket' value with any optional fields omitted.
mkBucket ::
  -- | 'creationDate'
  Core.UTCTime ->
  -- | 'name'
  Types.Name ->
  Bucket
mkBucket creationDate name = Bucket' {creationDate, name}

-- | Date the bucket was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCreationDate :: Lens.Lens' Bucket Core.UTCTime
bCreationDate = Lens.field @"creationDate"
{-# DEPRECATED bCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The name of the bucket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Bucket Types.Name
bName = Lens.field @"name"
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML Bucket where
  parseXML x =
    Bucket'
      Core.<$> (x Core..@ "CreationDate") Core.<*> (x Core..@ "Name")
