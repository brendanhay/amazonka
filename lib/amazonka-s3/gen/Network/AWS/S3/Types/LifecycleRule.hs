{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.LifecycleRule
  ( LifecycleRule (..)
  -- * Smart constructor
  , mkLifecycleRule
  -- * Lenses
  , lrStatus
  , lrAbortIncompleteMultipartUpload
  , lrExpiration
  , lrFilter
  , lrID
  , lrNoncurrentVersionExpiration
  , lrNoncurrentVersionTransitions
  , lrPrefix
  , lrTransitions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AbortIncompleteMultipartUpload as Types
import qualified Network.AWS.S3.Types.ExpirationStatus as Types
import qualified Network.AWS.S3.Types.ID as Types
import qualified Network.AWS.S3.Types.LifecycleExpiration as Types
import qualified Network.AWS.S3.Types.LifecycleRuleFilter as Types
import qualified Network.AWS.S3.Types.NoncurrentVersionExpiration as Types
import qualified Network.AWS.S3.Types.NoncurrentVersionTransition as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Transition as Types

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
-- /See:/ 'mkLifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { status :: Types.ExpirationStatus
    -- ^ If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
  , abortIncompleteMultipartUpload :: Core.Maybe Types.AbortIncompleteMultipartUpload
  , expiration :: Core.Maybe Types.LifecycleExpiration
    -- ^ Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
  , filter :: Core.Maybe Types.LifecycleRuleFilter
  , id :: Core.Maybe Types.ID
    -- ^ Unique identifier for the rule. The value cannot be longer than 255 characters.
  , noncurrentVersionExpiration :: Core.Maybe Types.NoncurrentVersionExpiration
  , noncurrentVersionTransitions :: Core.Maybe [Types.NoncurrentVersionTransition]
    -- ^ Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime. 
  , prefix :: Core.Maybe Types.Prefix
    -- ^ Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
  , transitions :: Core.Maybe [Types.Transition]
    -- ^ Specifies when an Amazon S3 object transitions to a specified storage class.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LifecycleRule' value with any optional fields omitted.
mkLifecycleRule
    :: Types.ExpirationStatus -- ^ 'status'
    -> LifecycleRule
mkLifecycleRule status
  = LifecycleRule'{status,
                   abortIncompleteMultipartUpload = Core.Nothing,
                   expiration = Core.Nothing, filter = Core.Nothing,
                   id = Core.Nothing, noncurrentVersionExpiration = Core.Nothing,
                   noncurrentVersionTransitions = Core.Nothing, prefix = Core.Nothing,
                   transitions = Core.Nothing}

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrStatus :: Lens.Lens' LifecycleRule Types.ExpirationStatus
lrStatus = Lens.field @"status"
{-# INLINEABLE lrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'abortIncompleteMultipartUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrAbortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Core.Maybe Types.AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = Lens.field @"abortIncompleteMultipartUpload"
{-# INLINEABLE lrAbortIncompleteMultipartUpload #-}
{-# DEPRECATED abortIncompleteMultipartUpload "Use generic-lens or generic-optics with 'abortIncompleteMultipartUpload' instead"  #-}

-- | Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrExpiration :: Lens.Lens' LifecycleRule (Core.Maybe Types.LifecycleExpiration)
lrExpiration = Lens.field @"expiration"
{-# INLINEABLE lrExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrFilter :: Lens.Lens' LifecycleRule (Core.Maybe Types.LifecycleRuleFilter)
lrFilter = Lens.field @"filter"
{-# INLINEABLE lrFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrID :: Lens.Lens' LifecycleRule (Core.Maybe Types.ID)
lrID = Lens.field @"id"
{-# INLINEABLE lrID #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'noncurrentVersionExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Core.Maybe Types.NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = Lens.field @"noncurrentVersionExpiration"
{-# INLINEABLE lrNoncurrentVersionExpiration #-}
{-# DEPRECATED noncurrentVersionExpiration "Use generic-lens or generic-optics with 'noncurrentVersionExpiration' instead"  #-}

-- | Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime. 
--
-- /Note:/ Consider using 'noncurrentVersionTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Core.Maybe [Types.NoncurrentVersionTransition])
lrNoncurrentVersionTransitions = Lens.field @"noncurrentVersionTransitions"
{-# INLINEABLE lrNoncurrentVersionTransitions #-}
{-# DEPRECATED noncurrentVersionTransitions "Use generic-lens or generic-optics with 'noncurrentVersionTransitions' instead"  #-}

-- | Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrPrefix :: Lens.Lens' LifecycleRule (Core.Maybe Types.Prefix)
lrPrefix = Lens.field @"prefix"
{-# INLINEABLE lrPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | Specifies when an Amazon S3 object transitions to a specified storage class.
--
-- /Note:/ Consider using 'transitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrTransitions :: Lens.Lens' LifecycleRule (Core.Maybe [Types.Transition])
lrTransitions = Lens.field @"transitions"
{-# INLINEABLE lrTransitions #-}
{-# DEPRECATED transitions "Use generic-lens or generic-optics with 'transitions' instead"  #-}

instance Core.ToXML LifecycleRule where
        toXML LifecycleRule{..}
          = Core.toXMLElement "Status" status Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "AbortIncompleteMultipartUpload")
                abortIncompleteMultipartUpload
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Expiration") expiration
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Filter") filter
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "ID") id
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "NoncurrentVersionExpiration")
                noncurrentVersionExpiration
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLList "NoncurrentVersionTransition")
                noncurrentVersionTransitions
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toXMLList "Transition") transitions

instance Core.FromXML LifecycleRule where
        parseXML x
          = LifecycleRule' Core.<$>
              (x Core..@ "Status") Core.<*>
                x Core..@? "AbortIncompleteMultipartUpload"
                Core.<*> x Core..@? "Expiration"
                Core.<*> x Core..@? "Filter"
                Core.<*> x Core..@? "ID"
                Core.<*> x Core..@? "NoncurrentVersionExpiration"
                Core.<*> x Core..@? "NoncurrentVersionTransition"
                Core.<*> x Core..@? "Prefix"
                Core.<*> x Core..@? "Transition"
