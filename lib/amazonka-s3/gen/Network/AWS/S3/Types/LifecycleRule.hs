{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRule
  ( LifecycleRule (..),

    -- * Smart constructor
    mkLifecycleRule,

    -- * Lenses
    lrStatus,
    lrAbortIncompleteMultipartUpload,
    lrExpiration,
    lrFilter,
    lrID,
    lrNoncurrentVersionExpiration,
    lrNoncurrentVersionTransitions,
    lrPrefix,
    lrTransitions,
  )
where

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
  { -- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
    status :: Types.ExpirationStatus,
    abortIncompleteMultipartUpload :: Core.Maybe Types.AbortIncompleteMultipartUpload,
    -- | Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
    expiration :: Core.Maybe Types.LifecycleExpiration,
    filter :: Core.Maybe Types.LifecycleRuleFilter,
    -- | Unique identifier for the rule. The value cannot be longer than 255 characters.
    id :: Core.Maybe Types.ID,
    noncurrentVersionExpiration :: Core.Maybe Types.NoncurrentVersionExpiration,
    -- | Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
    noncurrentVersionTransitions :: Core.Maybe [Types.NoncurrentVersionTransition],
    -- | Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
    prefix :: Core.Maybe Types.Prefix,
    -- | Specifies when an Amazon S3 object transitions to a specified storage class.
    transitions :: Core.Maybe [Types.Transition]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LifecycleRule' value with any optional fields omitted.
mkLifecycleRule ::
  -- | 'status'
  Types.ExpirationStatus ->
  LifecycleRule
mkLifecycleRule status =
  LifecycleRule'
    { status,
      abortIncompleteMultipartUpload = Core.Nothing,
      expiration = Core.Nothing,
      filter = Core.Nothing,
      id = Core.Nothing,
      noncurrentVersionExpiration = Core.Nothing,
      noncurrentVersionTransitions = Core.Nothing,
      prefix = Core.Nothing,
      transitions = Core.Nothing
    }

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrStatus :: Lens.Lens' LifecycleRule Types.ExpirationStatus
lrStatus = Lens.field @"status"
{-# DEPRECATED lrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'abortIncompleteMultipartUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrAbortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Core.Maybe Types.AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = Lens.field @"abortIncompleteMultipartUpload"
{-# DEPRECATED lrAbortIncompleteMultipartUpload "Use generic-lens or generic-optics with 'abortIncompleteMultipartUpload' instead." #-}

-- | Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrExpiration :: Lens.Lens' LifecycleRule (Core.Maybe Types.LifecycleExpiration)
lrExpiration = Lens.field @"expiration"
{-# DEPRECATED lrExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrFilter :: Lens.Lens' LifecycleRule (Core.Maybe Types.LifecycleRuleFilter)
lrFilter = Lens.field @"filter"
{-# DEPRECATED lrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrID :: Lens.Lens' LifecycleRule (Core.Maybe Types.ID)
lrID = Lens.field @"id"
{-# DEPRECATED lrID "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'noncurrentVersionExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Core.Maybe Types.NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = Lens.field @"noncurrentVersionExpiration"
{-# DEPRECATED lrNoncurrentVersionExpiration "Use generic-lens or generic-optics with 'noncurrentVersionExpiration' instead." #-}

-- | Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
--
-- /Note:/ Consider using 'noncurrentVersionTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Core.Maybe [Types.NoncurrentVersionTransition])
lrNoncurrentVersionTransitions = Lens.field @"noncurrentVersionTransitions"
{-# DEPRECATED lrNoncurrentVersionTransitions "Use generic-lens or generic-optics with 'noncurrentVersionTransitions' instead." #-}

-- | Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrPrefix :: Lens.Lens' LifecycleRule (Core.Maybe Types.Prefix)
lrPrefix = Lens.field @"prefix"
{-# DEPRECATED lrPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Specifies when an Amazon S3 object transitions to a specified storage class.
--
-- /Note:/ Consider using 'transitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrTransitions :: Lens.Lens' LifecycleRule (Core.Maybe [Types.Transition])
lrTransitions = Lens.field @"transitions"
{-# DEPRECATED lrTransitions "Use generic-lens or generic-optics with 'transitions' instead." #-}

instance Core.ToXML LifecycleRule where
  toXML LifecycleRule {..} =
    Core.toXMLNode "Status" status
      Core.<> Core.toXMLNode "AbortIncompleteMultipartUpload"
      Core.<$> abortIncompleteMultipartUpload
      Core.<> Core.toXMLNode "Expiration"
      Core.<$> expiration
      Core.<> Core.toXMLNode "Filter"
      Core.<$> filter
      Core.<> Core.toXMLNode "ID"
      Core.<$> id
      Core.<> Core.toXMLNode "NoncurrentVersionExpiration"
      Core.<$> noncurrentVersionExpiration
      Core.<> Core.toXMLList "NoncurrentVersionTransition"
      Core.<$> noncurrentVersionTransitions
      Core.<> Core.toXMLNode "Prefix"
      Core.<$> prefix
      Core.<> Core.toXMLList "Transition"
      Core.<$> transitions

instance Core.FromXML LifecycleRule where
  parseXML x =
    LifecycleRule'
      Core.<$> (x Core..@ "Status")
      Core.<*> (x Core..@? "AbortIncompleteMultipartUpload")
      Core.<*> (x Core..@? "Expiration")
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@? "ID")
      Core.<*> (x Core..@? "NoncurrentVersionExpiration")
      Core.<*> (x Core..@? "NoncurrentVersionTransition")
      Core.<*> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Transition")
