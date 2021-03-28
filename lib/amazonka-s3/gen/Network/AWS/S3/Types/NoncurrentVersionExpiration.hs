{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NoncurrentVersionExpiration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.NoncurrentVersionExpiration
  ( NoncurrentVersionExpiration (..)
  -- * Smart constructor
  , mkNoncurrentVersionExpiration
  -- * Lenses
  , nveNoncurrentDays
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
--
-- /See:/ 'mkNoncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { noncurrentDays :: Core.Int
    -- ^ Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NoncurrentVersionExpiration' value with any optional fields omitted.
mkNoncurrentVersionExpiration
    :: Core.Int -- ^ 'noncurrentDays'
    -> NoncurrentVersionExpiration
mkNoncurrentVersionExpiration noncurrentDays
  = NoncurrentVersionExpiration'{noncurrentDays}

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'noncurrentDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nveNoncurrentDays :: Lens.Lens' NoncurrentVersionExpiration Core.Int
nveNoncurrentDays = Lens.field @"noncurrentDays"
{-# INLINEABLE nveNoncurrentDays #-}
{-# DEPRECATED noncurrentDays "Use generic-lens or generic-optics with 'noncurrentDays' instead"  #-}

instance Core.ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration{..}
          = Core.toXMLElement "NoncurrentDays" noncurrentDays

instance Core.FromXML NoncurrentVersionExpiration where
        parseXML x
          = NoncurrentVersionExpiration' Core.<$>
              (x Core..@ "NoncurrentDays")
