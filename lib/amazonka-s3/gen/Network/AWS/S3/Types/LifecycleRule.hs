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
    lrTransitions,
    lrNoncurrentVersionExpiration,
    lrPrefix,
    lrNoncurrentVersionTransitions,
    lrExpiration,
    lrId,
    lrFilter,
    lrAbortIncompleteMultipartUpload,
    lrStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AbortIncompleteMultipartUpload
import Network.AWS.S3.Types.ExpirationStatus
import Network.AWS.S3.Types.LifecycleExpiration
import Network.AWS.S3.Types.LifecycleRuleFilter
import Network.AWS.S3.Types.NoncurrentVersionExpiration
import Network.AWS.S3.Types.NoncurrentVersionTransition
import Network.AWS.S3.Types.Transition

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
-- /See:/ 'mkLifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { transitions ::
      Lude.Maybe [Transition],
    noncurrentVersionExpiration ::
      Lude.Maybe NoncurrentVersionExpiration,
    prefix :: Lude.Maybe Lude.Text,
    noncurrentVersionTransitions ::
      Lude.Maybe [NoncurrentVersionTransition],
    expiration :: Lude.Maybe LifecycleExpiration,
    id :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe LifecycleRuleFilter,
    abortIncompleteMultipartUpload ::
      Lude.Maybe AbortIncompleteMultipartUpload,
    status :: ExpirationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleRule' with the minimum fields required to make a request.
--
-- * 'abortIncompleteMultipartUpload' - Undocumented field.
-- * 'expiration' - Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
-- * 'filter' - Undocumented field.
-- * 'id' - Unique identifier for the rule. The value cannot be longer than 255 characters.
-- * 'noncurrentVersionExpiration' - Undocumented field.
-- * 'noncurrentVersionTransitions' - Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
-- * 'prefix' - Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
-- * 'status' - If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
-- * 'transitions' - Specifies when an Amazon S3 object transitions to a specified storage class.
mkLifecycleRule ::
  -- | 'status'
  ExpirationStatus ->
  LifecycleRule
mkLifecycleRule pStatus_ =
  LifecycleRule'
    { transitions = Lude.Nothing,
      noncurrentVersionExpiration = Lude.Nothing,
      prefix = Lude.Nothing,
      noncurrentVersionTransitions = Lude.Nothing,
      expiration = Lude.Nothing,
      id = Lude.Nothing,
      filter = Lude.Nothing,
      abortIncompleteMultipartUpload = Lude.Nothing,
      status = pStatus_
    }

-- | Specifies when an Amazon S3 object transitions to a specified storage class.
--
-- /Note:/ Consider using 'transitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrTransitions :: Lens.Lens' LifecycleRule (Lude.Maybe [Transition])
lrTransitions = Lens.lens (transitions :: LifecycleRule -> Lude.Maybe [Transition]) (\s a -> s {transitions = a} :: LifecycleRule)
{-# DEPRECATED lrTransitions "Use generic-lens or generic-optics with 'transitions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'noncurrentVersionExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Lude.Maybe NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = Lens.lens (noncurrentVersionExpiration :: LifecycleRule -> Lude.Maybe NoncurrentVersionExpiration) (\s a -> s {noncurrentVersionExpiration = a} :: LifecycleRule)
{-# DEPRECATED lrNoncurrentVersionExpiration "Use generic-lens or generic-optics with 'noncurrentVersionExpiration' instead." #-}

-- | Prefix identifying one or more objects to which the rule applies. This is No longer used; use @Filter@ instead.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrPrefix :: Lens.Lens' LifecycleRule (Lude.Maybe Lude.Text)
lrPrefix = Lens.lens (prefix :: LifecycleRule -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: LifecycleRule)
{-# DEPRECATED lrPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Specifies the transition rule for the lifecycle rule that describes when noncurrent objects transition to a specific storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to a specific storage class at a set period in the object's lifetime.
--
-- /Note:/ Consider using 'noncurrentVersionTransitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNoncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Lude.Maybe [NoncurrentVersionTransition])
lrNoncurrentVersionTransitions = Lens.lens (noncurrentVersionTransitions :: LifecycleRule -> Lude.Maybe [NoncurrentVersionTransition]) (\s a -> s {noncurrentVersionTransitions = a} :: LifecycleRule)
{-# DEPRECATED lrNoncurrentVersionTransitions "Use generic-lens or generic-optics with 'noncurrentVersionTransitions' instead." #-}

-- | Specifies the expiration for the lifecycle of the object in the form of date, days and, whether the object has a delete marker.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrExpiration :: Lens.Lens' LifecycleRule (Lude.Maybe LifecycleExpiration)
lrExpiration = Lens.lens (expiration :: LifecycleRule -> Lude.Maybe LifecycleExpiration) (\s a -> s {expiration = a} :: LifecycleRule)
{-# DEPRECATED lrExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrId :: Lens.Lens' LifecycleRule (Lude.Maybe Lude.Text)
lrId = Lens.lens (id :: LifecycleRule -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: LifecycleRule)
{-# DEPRECATED lrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrFilter :: Lens.Lens' LifecycleRule (Lude.Maybe LifecycleRuleFilter)
lrFilter = Lens.lens (filter :: LifecycleRule -> Lude.Maybe LifecycleRuleFilter) (\s a -> s {filter = a} :: LifecycleRule)
{-# DEPRECATED lrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'abortIncompleteMultipartUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrAbortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Lude.Maybe AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = Lens.lens (abortIncompleteMultipartUpload :: LifecycleRule -> Lude.Maybe AbortIncompleteMultipartUpload) (\s a -> s {abortIncompleteMultipartUpload = a} :: LifecycleRule)
{-# DEPRECATED lrAbortIncompleteMultipartUpload "Use generic-lens or generic-optics with 'abortIncompleteMultipartUpload' instead." #-}

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrStatus :: Lens.Lens' LifecycleRule ExpirationStatus
lrStatus = Lens.lens (status :: LifecycleRule -> ExpirationStatus) (\s a -> s {status = a} :: LifecycleRule)
{-# DEPRECATED lrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML LifecycleRule where
  parseXML x =
    LifecycleRule'
      Lude.<$> (Lude.may (Lude.parseXMLList "Transition") x)
      Lude.<*> (x Lude..@? "NoncurrentVersionExpiration")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (Lude.may (Lude.parseXMLList "NoncurrentVersionTransition") x)
      Lude.<*> (x Lude..@? "Expiration")
      Lude.<*> (x Lude..@? "ID")
      Lude.<*> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@? "AbortIncompleteMultipartUpload")
      Lude.<*> (x Lude..@ "Status")

instance Lude.ToXML LifecycleRule where
  toXML LifecycleRule' {..} =
    Lude.mconcat
      [ Lude.toXML (Lude.toXMLList "Transition" Lude.<$> transitions),
        "NoncurrentVersionExpiration" Lude.@= noncurrentVersionExpiration,
        "Prefix" Lude.@= prefix,
        Lude.toXML
          ( Lude.toXMLList "NoncurrentVersionTransition"
              Lude.<$> noncurrentVersionTransitions
          ),
        "Expiration" Lude.@= expiration,
        "ID" Lude.@= id,
        "Filter" Lude.@= filter,
        "AbortIncompleteMultipartUpload"
          Lude.@= abortIncompleteMultipartUpload,
        "Status" Lude.@= status
      ]
