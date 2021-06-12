{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
-- /See:/ 'newLifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { -- | Specifies the expiration for the lifecycle of the object in the form of
    -- date, days and, whether the object has a delete marker.
    expiration :: Core.Maybe LifecycleExpiration,
    -- | Prefix identifying one or more objects to which the rule applies. This
    -- is No longer used; use @Filter@ instead.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Core.Maybe Core.Text,
    -- | Specifies the transition rule for the lifecycle rule that describes when
    -- noncurrent objects transition to a specific storage class. If your
    -- bucket is versioning-enabled (or versioning is suspended), you can set
    -- this action to request that Amazon S3 transition noncurrent object
    -- versions to a specific storage class at a set period in the object\'s
    -- lifetime.
    noncurrentVersionTransitions :: Core.Maybe [NoncurrentVersionTransition],
    -- | Unique identifier for the rule. The value cannot be longer than 255
    -- characters.
    id :: Core.Maybe Core.Text,
    noncurrentVersionExpiration :: Core.Maybe NoncurrentVersionExpiration,
    -- | Specifies when an Amazon S3 object transitions to a specified storage
    -- class.
    transitions :: Core.Maybe [Transition],
    abortIncompleteMultipartUpload :: Core.Maybe AbortIncompleteMultipartUpload,
    filter' :: Core.Maybe LifecycleRuleFilter,
    -- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
    -- the rule is not currently being applied.
    status :: ExpirationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LifecycleRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'lifecycleRule_expiration' - Specifies the expiration for the lifecycle of the object in the form of
-- date, days and, whether the object has a delete marker.
--
-- 'prefix', 'lifecycleRule_prefix' - Prefix identifying one or more objects to which the rule applies. This
-- is No longer used; use @Filter@ instead.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'noncurrentVersionTransitions', 'lifecycleRule_noncurrentVersionTransitions' - Specifies the transition rule for the lifecycle rule that describes when
-- noncurrent objects transition to a specific storage class. If your
-- bucket is versioning-enabled (or versioning is suspended), you can set
-- this action to request that Amazon S3 transition noncurrent object
-- versions to a specific storage class at a set period in the object\'s
-- lifetime.
--
-- 'id', 'lifecycleRule_id' - Unique identifier for the rule. The value cannot be longer than 255
-- characters.
--
-- 'noncurrentVersionExpiration', 'lifecycleRule_noncurrentVersionExpiration' - Undocumented member.
--
-- 'transitions', 'lifecycleRule_transitions' - Specifies when an Amazon S3 object transitions to a specified storage
-- class.
--
-- 'abortIncompleteMultipartUpload', 'lifecycleRule_abortIncompleteMultipartUpload' - Undocumented member.
--
-- 'filter'', 'lifecycleRule_filter' - Undocumented member.
--
-- 'status', 'lifecycleRule_status' - If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
newLifecycleRule ::
  -- | 'status'
  ExpirationStatus ->
  LifecycleRule
newLifecycleRule pStatus_ =
  LifecycleRule'
    { expiration = Core.Nothing,
      prefix = Core.Nothing,
      noncurrentVersionTransitions = Core.Nothing,
      id = Core.Nothing,
      noncurrentVersionExpiration = Core.Nothing,
      transitions = Core.Nothing,
      abortIncompleteMultipartUpload = Core.Nothing,
      filter' = Core.Nothing,
      status = pStatus_
    }

-- | Specifies the expiration for the lifecycle of the object in the form of
-- date, days and, whether the object has a delete marker.
lifecycleRule_expiration :: Lens.Lens' LifecycleRule (Core.Maybe LifecycleExpiration)
lifecycleRule_expiration = Lens.lens (\LifecycleRule' {expiration} -> expiration) (\s@LifecycleRule' {} a -> s {expiration = a} :: LifecycleRule)

-- | Prefix identifying one or more objects to which the rule applies. This
-- is No longer used; use @Filter@ instead.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRule_prefix :: Lens.Lens' LifecycleRule (Core.Maybe Core.Text)
lifecycleRule_prefix = Lens.lens (\LifecycleRule' {prefix} -> prefix) (\s@LifecycleRule' {} a -> s {prefix = a} :: LifecycleRule)

-- | Specifies the transition rule for the lifecycle rule that describes when
-- noncurrent objects transition to a specific storage class. If your
-- bucket is versioning-enabled (or versioning is suspended), you can set
-- this action to request that Amazon S3 transition noncurrent object
-- versions to a specific storage class at a set period in the object\'s
-- lifetime.
lifecycleRule_noncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Core.Maybe [NoncurrentVersionTransition])
lifecycleRule_noncurrentVersionTransitions = Lens.lens (\LifecycleRule' {noncurrentVersionTransitions} -> noncurrentVersionTransitions) (\s@LifecycleRule' {} a -> s {noncurrentVersionTransitions = a} :: LifecycleRule) Core.. Lens.mapping Lens._Coerce

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
lifecycleRule_id :: Lens.Lens' LifecycleRule (Core.Maybe Core.Text)
lifecycleRule_id = Lens.lens (\LifecycleRule' {id} -> id) (\s@LifecycleRule' {} a -> s {id = a} :: LifecycleRule)

-- | Undocumented member.
lifecycleRule_noncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Core.Maybe NoncurrentVersionExpiration)
lifecycleRule_noncurrentVersionExpiration = Lens.lens (\LifecycleRule' {noncurrentVersionExpiration} -> noncurrentVersionExpiration) (\s@LifecycleRule' {} a -> s {noncurrentVersionExpiration = a} :: LifecycleRule)

-- | Specifies when an Amazon S3 object transitions to a specified storage
-- class.
lifecycleRule_transitions :: Lens.Lens' LifecycleRule (Core.Maybe [Transition])
lifecycleRule_transitions = Lens.lens (\LifecycleRule' {transitions} -> transitions) (\s@LifecycleRule' {} a -> s {transitions = a} :: LifecycleRule) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
lifecycleRule_abortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Core.Maybe AbortIncompleteMultipartUpload)
lifecycleRule_abortIncompleteMultipartUpload = Lens.lens (\LifecycleRule' {abortIncompleteMultipartUpload} -> abortIncompleteMultipartUpload) (\s@LifecycleRule' {} a -> s {abortIncompleteMultipartUpload = a} :: LifecycleRule)

-- | Undocumented member.
lifecycleRule_filter :: Lens.Lens' LifecycleRule (Core.Maybe LifecycleRuleFilter)
lifecycleRule_filter = Lens.lens (\LifecycleRule' {filter'} -> filter') (\s@LifecycleRule' {} a -> s {filter' = a} :: LifecycleRule)

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
lifecycleRule_status :: Lens.Lens' LifecycleRule ExpirationStatus
lifecycleRule_status = Lens.lens (\LifecycleRule' {status} -> status) (\s@LifecycleRule' {} a -> s {status = a} :: LifecycleRule)

instance Core.FromXML LifecycleRule where
  parseXML x =
    LifecycleRule'
      Core.<$> (x Core..@? "Expiration")
      Core.<*> (x Core..@? "Prefix")
      Core.<*> ( Core.may
                   (Core.parseXMLList "NoncurrentVersionTransition")
                   x
               )
      Core.<*> (x Core..@? "ID")
      Core.<*> (x Core..@? "NoncurrentVersionExpiration")
      Core.<*> (Core.may (Core.parseXMLList "Transition") x)
      Core.<*> (x Core..@? "AbortIncompleteMultipartUpload")
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@ "Status")

instance Core.Hashable LifecycleRule

instance Core.NFData LifecycleRule

instance Core.ToXML LifecycleRule where
  toXML LifecycleRule' {..} =
    Core.mconcat
      [ "Expiration" Core.@= expiration,
        "Prefix" Core.@= prefix,
        Core.toXML
          ( Core.toXMLList "NoncurrentVersionTransition"
              Core.<$> noncurrentVersionTransitions
          ),
        "ID" Core.@= id,
        "NoncurrentVersionExpiration"
          Core.@= noncurrentVersionExpiration,
        Core.toXML
          (Core.toXMLList "Transition" Core.<$> transitions),
        "AbortIncompleteMultipartUpload"
          Core.@= abortIncompleteMultipartUpload,
        "Filter" Core.@= filter',
        "Status" Core.@= status
      ]
