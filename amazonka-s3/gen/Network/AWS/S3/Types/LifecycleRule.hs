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
import qualified Network.AWS.Prelude as Prelude
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
    expiration :: Prelude.Maybe LifecycleExpiration,
    -- | Prefix identifying one or more objects to which the rule applies. This
    -- is No longer used; use @Filter@ instead.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the transition rule for the lifecycle rule that describes when
    -- noncurrent objects transition to a specific storage class. If your
    -- bucket is versioning-enabled (or versioning is suspended), you can set
    -- this action to request that Amazon S3 transition noncurrent object
    -- versions to a specific storage class at a set period in the object\'s
    -- lifetime.
    noncurrentVersionTransitions :: Prelude.Maybe [NoncurrentVersionTransition],
    -- | Unique identifier for the rule. The value cannot be longer than 255
    -- characters.
    id :: Prelude.Maybe Prelude.Text,
    noncurrentVersionExpiration :: Prelude.Maybe NoncurrentVersionExpiration,
    -- | Specifies when an Amazon S3 object transitions to a specified storage
    -- class.
    transitions :: Prelude.Maybe [Transition],
    abortIncompleteMultipartUpload :: Prelude.Maybe AbortIncompleteMultipartUpload,
    filter' :: Prelude.Maybe LifecycleRuleFilter,
    -- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
    -- the rule is not currently being applied.
    status :: ExpirationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { expiration = Prelude.Nothing,
      prefix = Prelude.Nothing,
      noncurrentVersionTransitions = Prelude.Nothing,
      id = Prelude.Nothing,
      noncurrentVersionExpiration = Prelude.Nothing,
      transitions = Prelude.Nothing,
      abortIncompleteMultipartUpload = Prelude.Nothing,
      filter' = Prelude.Nothing,
      status = pStatus_
    }

-- | Specifies the expiration for the lifecycle of the object in the form of
-- date, days and, whether the object has a delete marker.
lifecycleRule_expiration :: Lens.Lens' LifecycleRule (Prelude.Maybe LifecycleExpiration)
lifecycleRule_expiration = Lens.lens (\LifecycleRule' {expiration} -> expiration) (\s@LifecycleRule' {} a -> s {expiration = a} :: LifecycleRule)

-- | Prefix identifying one or more objects to which the rule applies. This
-- is No longer used; use @Filter@ instead.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRule_prefix :: Lens.Lens' LifecycleRule (Prelude.Maybe Prelude.Text)
lifecycleRule_prefix = Lens.lens (\LifecycleRule' {prefix} -> prefix) (\s@LifecycleRule' {} a -> s {prefix = a} :: LifecycleRule)

-- | Specifies the transition rule for the lifecycle rule that describes when
-- noncurrent objects transition to a specific storage class. If your
-- bucket is versioning-enabled (or versioning is suspended), you can set
-- this action to request that Amazon S3 transition noncurrent object
-- versions to a specific storage class at a set period in the object\'s
-- lifetime.
lifecycleRule_noncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Prelude.Maybe [NoncurrentVersionTransition])
lifecycleRule_noncurrentVersionTransitions = Lens.lens (\LifecycleRule' {noncurrentVersionTransitions} -> noncurrentVersionTransitions) (\s@LifecycleRule' {} a -> s {noncurrentVersionTransitions = a} :: LifecycleRule) Prelude.. Lens.mapping Lens._Coerce

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
lifecycleRule_id :: Lens.Lens' LifecycleRule (Prelude.Maybe Prelude.Text)
lifecycleRule_id = Lens.lens (\LifecycleRule' {id} -> id) (\s@LifecycleRule' {} a -> s {id = a} :: LifecycleRule)

-- | Undocumented member.
lifecycleRule_noncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Prelude.Maybe NoncurrentVersionExpiration)
lifecycleRule_noncurrentVersionExpiration = Lens.lens (\LifecycleRule' {noncurrentVersionExpiration} -> noncurrentVersionExpiration) (\s@LifecycleRule' {} a -> s {noncurrentVersionExpiration = a} :: LifecycleRule)

-- | Specifies when an Amazon S3 object transitions to a specified storage
-- class.
lifecycleRule_transitions :: Lens.Lens' LifecycleRule (Prelude.Maybe [Transition])
lifecycleRule_transitions = Lens.lens (\LifecycleRule' {transitions} -> transitions) (\s@LifecycleRule' {} a -> s {transitions = a} :: LifecycleRule) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
lifecycleRule_abortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Prelude.Maybe AbortIncompleteMultipartUpload)
lifecycleRule_abortIncompleteMultipartUpload = Lens.lens (\LifecycleRule' {abortIncompleteMultipartUpload} -> abortIncompleteMultipartUpload) (\s@LifecycleRule' {} a -> s {abortIncompleteMultipartUpload = a} :: LifecycleRule)

-- | Undocumented member.
lifecycleRule_filter :: Lens.Lens' LifecycleRule (Prelude.Maybe LifecycleRuleFilter)
lifecycleRule_filter = Lens.lens (\LifecycleRule' {filter'} -> filter') (\s@LifecycleRule' {} a -> s {filter' = a} :: LifecycleRule)

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
lifecycleRule_status :: Lens.Lens' LifecycleRule ExpirationStatus
lifecycleRule_status = Lens.lens (\LifecycleRule' {status} -> status) (\s@LifecycleRule' {} a -> s {status = a} :: LifecycleRule)

instance Core.FromXML LifecycleRule where
  parseXML x =
    LifecycleRule'
      Prelude.<$> (x Core..@? "Expiration")
      Prelude.<*> (x Core..@? "Prefix")
      Prelude.<*> ( Core.may
                      (Core.parseXMLList "NoncurrentVersionTransition")
                      x
                  )
      Prelude.<*> (x Core..@? "ID")
      Prelude.<*> (x Core..@? "NoncurrentVersionExpiration")
      Prelude.<*> (Core.may (Core.parseXMLList "Transition") x)
      Prelude.<*> (x Core..@? "AbortIncompleteMultipartUpload")
      Prelude.<*> (x Core..@? "Filter")
      Prelude.<*> (x Core..@ "Status")

instance Prelude.Hashable LifecycleRule

instance Prelude.NFData LifecycleRule

instance Core.ToXML LifecycleRule where
  toXML LifecycleRule' {..} =
    Prelude.mconcat
      [ "Expiration" Core.@= expiration,
        "Prefix" Core.@= prefix,
        Core.toXML
          ( Core.toXMLList "NoncurrentVersionTransition"
              Prelude.<$> noncurrentVersionTransitions
          ),
        "ID" Core.@= id,
        "NoncurrentVersionExpiration"
          Core.@= noncurrentVersionExpiration,
        Core.toXML
          ( Core.toXMLList "Transition"
              Prelude.<$> transitions
          ),
        "AbortIncompleteMultipartUpload"
          Core.@= abortIncompleteMultipartUpload,
        "Filter" Core.@= filter',
        "Status" Core.@= status
      ]
