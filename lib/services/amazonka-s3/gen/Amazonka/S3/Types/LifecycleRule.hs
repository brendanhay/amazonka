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
-- Module      : Amazonka.S3.Types.LifecycleRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.LifecycleRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.AbortIncompleteMultipartUpload
import Amazonka.S3.Types.ExpirationStatus
import Amazonka.S3.Types.LifecycleExpiration
import Amazonka.S3.Types.LifecycleRuleFilter
import Amazonka.S3.Types.NoncurrentVersionExpiration
import Amazonka.S3.Types.NoncurrentVersionTransition
import Amazonka.S3.Types.Transition

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
--
-- /See:/ 'newLifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { abortIncompleteMultipartUpload :: Prelude.Maybe AbortIncompleteMultipartUpload,
    -- | Specifies the expiration for the lifecycle of the object in the form of
    -- date, days and, whether the object has a delete marker.
    expiration :: Prelude.Maybe LifecycleExpiration,
    -- | The @Filter@ is used to identify objects that a Lifecycle Rule applies
    -- to. A @Filter@ must have exactly one of @Prefix@, @Tag@, or @And@
    -- specified. @Filter@ is required if the @LifecycleRule@ does not contain
    -- a @Prefix@ element.
    filter' :: Prelude.Maybe LifecycleRuleFilter,
    -- | Unique identifier for the rule. The value cannot be longer than 255
    -- characters.
    id :: Prelude.Maybe Prelude.Text,
    noncurrentVersionExpiration :: Prelude.Maybe NoncurrentVersionExpiration,
    -- | Specifies the transition rule for the lifecycle rule that describes when
    -- noncurrent objects transition to a specific storage class. If your
    -- bucket is versioning-enabled (or versioning is suspended), you can set
    -- this action to request that Amazon S3 transition noncurrent object
    -- versions to a specific storage class at a set period in the object\'s
    -- lifetime.
    noncurrentVersionTransitions :: Prelude.Maybe [NoncurrentVersionTransition],
    -- | Prefix identifying one or more objects to which the rule applies. This
    -- is no longer used; use @Filter@ instead.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies when an Amazon S3 object transitions to a specified storage
    -- class.
    transitions :: Prelude.Maybe [Transition],
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
-- 'abortIncompleteMultipartUpload', 'lifecycleRule_abortIncompleteMultipartUpload' - Undocumented member.
--
-- 'expiration', 'lifecycleRule_expiration' - Specifies the expiration for the lifecycle of the object in the form of
-- date, days and, whether the object has a delete marker.
--
-- 'filter'', 'lifecycleRule_filter' - The @Filter@ is used to identify objects that a Lifecycle Rule applies
-- to. A @Filter@ must have exactly one of @Prefix@, @Tag@, or @And@
-- specified. @Filter@ is required if the @LifecycleRule@ does not contain
-- a @Prefix@ element.
--
-- 'id', 'lifecycleRule_id' - Unique identifier for the rule. The value cannot be longer than 255
-- characters.
--
-- 'noncurrentVersionExpiration', 'lifecycleRule_noncurrentVersionExpiration' - Undocumented member.
--
-- 'noncurrentVersionTransitions', 'lifecycleRule_noncurrentVersionTransitions' - Specifies the transition rule for the lifecycle rule that describes when
-- noncurrent objects transition to a specific storage class. If your
-- bucket is versioning-enabled (or versioning is suspended), you can set
-- this action to request that Amazon S3 transition noncurrent object
-- versions to a specific storage class at a set period in the object\'s
-- lifetime.
--
-- 'prefix', 'lifecycleRule_prefix' - Prefix identifying one or more objects to which the rule applies. This
-- is no longer used; use @Filter@ instead.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'transitions', 'lifecycleRule_transitions' - Specifies when an Amazon S3 object transitions to a specified storage
-- class.
--
-- 'status', 'lifecycleRule_status' - If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
newLifecycleRule ::
  -- | 'status'
  ExpirationStatus ->
  LifecycleRule
newLifecycleRule pStatus_ =
  LifecycleRule'
    { abortIncompleteMultipartUpload =
        Prelude.Nothing,
      expiration = Prelude.Nothing,
      filter' = Prelude.Nothing,
      id = Prelude.Nothing,
      noncurrentVersionExpiration = Prelude.Nothing,
      noncurrentVersionTransitions = Prelude.Nothing,
      prefix = Prelude.Nothing,
      transitions = Prelude.Nothing,
      status = pStatus_
    }

-- | Undocumented member.
lifecycleRule_abortIncompleteMultipartUpload :: Lens.Lens' LifecycleRule (Prelude.Maybe AbortIncompleteMultipartUpload)
lifecycleRule_abortIncompleteMultipartUpload = Lens.lens (\LifecycleRule' {abortIncompleteMultipartUpload} -> abortIncompleteMultipartUpload) (\s@LifecycleRule' {} a -> s {abortIncompleteMultipartUpload = a} :: LifecycleRule)

-- | Specifies the expiration for the lifecycle of the object in the form of
-- date, days and, whether the object has a delete marker.
lifecycleRule_expiration :: Lens.Lens' LifecycleRule (Prelude.Maybe LifecycleExpiration)
lifecycleRule_expiration = Lens.lens (\LifecycleRule' {expiration} -> expiration) (\s@LifecycleRule' {} a -> s {expiration = a} :: LifecycleRule)

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies
-- to. A @Filter@ must have exactly one of @Prefix@, @Tag@, or @And@
-- specified. @Filter@ is required if the @LifecycleRule@ does not contain
-- a @Prefix@ element.
lifecycleRule_filter :: Lens.Lens' LifecycleRule (Prelude.Maybe LifecycleRuleFilter)
lifecycleRule_filter = Lens.lens (\LifecycleRule' {filter'} -> filter') (\s@LifecycleRule' {} a -> s {filter' = a} :: LifecycleRule)

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
lifecycleRule_id :: Lens.Lens' LifecycleRule (Prelude.Maybe Prelude.Text)
lifecycleRule_id = Lens.lens (\LifecycleRule' {id} -> id) (\s@LifecycleRule' {} a -> s {id = a} :: LifecycleRule)

-- | Undocumented member.
lifecycleRule_noncurrentVersionExpiration :: Lens.Lens' LifecycleRule (Prelude.Maybe NoncurrentVersionExpiration)
lifecycleRule_noncurrentVersionExpiration = Lens.lens (\LifecycleRule' {noncurrentVersionExpiration} -> noncurrentVersionExpiration) (\s@LifecycleRule' {} a -> s {noncurrentVersionExpiration = a} :: LifecycleRule)

-- | Specifies the transition rule for the lifecycle rule that describes when
-- noncurrent objects transition to a specific storage class. If your
-- bucket is versioning-enabled (or versioning is suspended), you can set
-- this action to request that Amazon S3 transition noncurrent object
-- versions to a specific storage class at a set period in the object\'s
-- lifetime.
lifecycleRule_noncurrentVersionTransitions :: Lens.Lens' LifecycleRule (Prelude.Maybe [NoncurrentVersionTransition])
lifecycleRule_noncurrentVersionTransitions = Lens.lens (\LifecycleRule' {noncurrentVersionTransitions} -> noncurrentVersionTransitions) (\s@LifecycleRule' {} a -> s {noncurrentVersionTransitions = a} :: LifecycleRule) Prelude.. Lens.mapping Lens.coerced

-- | Prefix identifying one or more objects to which the rule applies. This
-- is no longer used; use @Filter@ instead.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
lifecycleRule_prefix :: Lens.Lens' LifecycleRule (Prelude.Maybe Prelude.Text)
lifecycleRule_prefix = Lens.lens (\LifecycleRule' {prefix} -> prefix) (\s@LifecycleRule' {} a -> s {prefix = a} :: LifecycleRule)

-- | Specifies when an Amazon S3 object transitions to a specified storage
-- class.
lifecycleRule_transitions :: Lens.Lens' LifecycleRule (Prelude.Maybe [Transition])
lifecycleRule_transitions = Lens.lens (\LifecycleRule' {transitions} -> transitions) (\s@LifecycleRule' {} a -> s {transitions = a} :: LifecycleRule) Prelude.. Lens.mapping Lens.coerced

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
lifecycleRule_status :: Lens.Lens' LifecycleRule ExpirationStatus
lifecycleRule_status = Lens.lens (\LifecycleRule' {status} -> status) (\s@LifecycleRule' {} a -> s {status = a} :: LifecycleRule)

instance Data.FromXML LifecycleRule where
  parseXML x =
    LifecycleRule'
      Prelude.<$> (x Data..@? "AbortIncompleteMultipartUpload")
      Prelude.<*> (x Data..@? "Expiration")
      Prelude.<*> (x Data..@? "Filter")
      Prelude.<*> (x Data..@? "ID")
      Prelude.<*> (x Data..@? "NoncurrentVersionExpiration")
      Prelude.<*> ( Core.may
                      (Data.parseXMLList "NoncurrentVersionTransition")
                      x
                  )
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> (Core.may (Data.parseXMLList "Transition") x)
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable LifecycleRule where
  hashWithSalt _salt LifecycleRule' {..} =
    _salt
      `Prelude.hashWithSalt` abortIncompleteMultipartUpload
      `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` noncurrentVersionExpiration
      `Prelude.hashWithSalt` noncurrentVersionTransitions
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` transitions
      `Prelude.hashWithSalt` status

instance Prelude.NFData LifecycleRule where
  rnf LifecycleRule' {..} =
    Prelude.rnf abortIncompleteMultipartUpload
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf noncurrentVersionExpiration
      `Prelude.seq` Prelude.rnf noncurrentVersionTransitions
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf transitions
      `Prelude.seq` Prelude.rnf status

instance Data.ToXML LifecycleRule where
  toXML LifecycleRule' {..} =
    Prelude.mconcat
      [ "AbortIncompleteMultipartUpload"
          Data.@= abortIncompleteMultipartUpload,
        "Expiration" Data.@= expiration,
        "Filter" Data.@= filter',
        "ID" Data.@= id,
        "NoncurrentVersionExpiration"
          Data.@= noncurrentVersionExpiration,
        Data.toXML
          ( Data.toXMLList "NoncurrentVersionTransition"
              Prelude.<$> noncurrentVersionTransitions
          ),
        "Prefix" Data.@= prefix,
        Data.toXML
          ( Data.toXMLList "Transition"
              Prelude.<$> transitions
          ),
        "Status" Data.@= status
      ]
