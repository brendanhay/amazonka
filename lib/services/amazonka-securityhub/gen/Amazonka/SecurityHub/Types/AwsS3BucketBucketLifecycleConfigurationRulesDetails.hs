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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails

-- | Configuration for a lifecycle rule.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesDetails = AwsS3BucketBucketLifecycleConfigurationRulesDetails'
  { -- | Transition rules that indicate when objects transition to a specified
    -- storage class.
    transitions :: Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails],
    -- | Whether Amazon S3 removes a delete marker that has no noncurrent
    -- versions. If set to @true@, the delete marker is expired. If set to
    -- @false@, the policy takes no action.
    --
    -- If you provide @ExpiredObjectDeleteMarker@, you cannot provide
    -- @ExpirationInDays@ or @ExpirationDate@.
    expiredObjectDeleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the rule. Indicates whether the rule is currently
    -- being applied.
    status :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the rule.
    id :: Prelude.Maybe Prelude.Text,
    -- | Identifies the objects that a rule applies to.
    filter' :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails,
    -- | The length in days of the lifetime for objects that are subject to the
    -- rule.
    expirationInDays :: Prelude.Maybe Prelude.Int,
    -- | Transition rules that describe when noncurrent objects transition to a
    -- specified storage class.
    noncurrentVersionTransitions :: Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails],
    -- | How Amazon S3 responds when a multipart upload is incomplete.
    -- Specifically, provides a number of days before Amazon S3 cancels the
    -- entire upload.
    abortIncompleteMultipartUpload :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails,
    -- | A prefix that identifies one or more objects that the rule applies to.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The date when objects are moved or deleted.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    expirationDate :: Prelude.Maybe Prelude.Text,
    -- | The number of days that an object is noncurrent before Amazon S3 can
    -- perform the associated action.
    noncurrentVersionExpirationInDays :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitions', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions' - Transition rules that indicate when objects transition to a specified
-- storage class.
--
-- 'expiredObjectDeleteMarker', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker' - Whether Amazon S3 removes a delete marker that has no noncurrent
-- versions. If set to @true@, the delete marker is expired. If set to
-- @false@, the policy takes no action.
--
-- If you provide @ExpiredObjectDeleteMarker@, you cannot provide
-- @ExpirationInDays@ or @ExpirationDate@.
--
-- 'status', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_status' - The current status of the rule. Indicates whether the rule is currently
-- being applied.
--
-- 'id', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_id' - The unique identifier of the rule.
--
-- 'filter'', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_filter' - Identifies the objects that a rule applies to.
--
-- 'expirationInDays', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays' - The length in days of the lifetime for objects that are subject to the
-- rule.
--
-- 'noncurrentVersionTransitions', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions' - Transition rules that describe when noncurrent objects transition to a
-- specified storage class.
--
-- 'abortIncompleteMultipartUpload', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload' - How Amazon S3 responds when a multipart upload is incomplete.
-- Specifically, provides a number of days before Amazon S3 cancels the
-- entire upload.
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix' - A prefix that identifies one or more objects that the rule applies to.
--
-- 'expirationDate', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate' - The date when objects are moved or deleted.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'noncurrentVersionExpirationInDays', 'awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays' - The number of days that an object is noncurrent before Amazon S3 can
-- perform the associated action.
newAwsS3BucketBucketLifecycleConfigurationRulesDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesDetails
newAwsS3BucketBucketLifecycleConfigurationRulesDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesDetails'
    { transitions =
        Prelude.Nothing,
      expiredObjectDeleteMarker =
        Prelude.Nothing,
      status =
        Prelude.Nothing,
      id = Prelude.Nothing,
      filter' =
        Prelude.Nothing,
      expirationInDays =
        Prelude.Nothing,
      noncurrentVersionTransitions =
        Prelude.Nothing,
      abortIncompleteMultipartUpload =
        Prelude.Nothing,
      prefix =
        Prelude.Nothing,
      expirationDate =
        Prelude.Nothing,
      noncurrentVersionExpirationInDays =
        Prelude.Nothing
    }

-- | Transition rules that indicate when objects transition to a specified
-- storage class.
awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails])
awsS3BucketBucketLifecycleConfigurationRulesDetails_transitions = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {transitions} -> transitions) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {transitions = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether Amazon S3 removes a delete marker that has no noncurrent
-- versions. If set to @true@, the delete marker is expired. If set to
-- @false@, the policy takes no action.
--
-- If you provide @ExpiredObjectDeleteMarker@, you cannot provide
-- @ExpirationInDays@ or @ExpirationDate@.
awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Bool)
awsS3BucketBucketLifecycleConfigurationRulesDetails_expiredObjectDeleteMarker = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {expiredObjectDeleteMarker} -> expiredObjectDeleteMarker) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {expiredObjectDeleteMarker = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | The current status of the rule. Indicates whether the rule is currently
-- being applied.
awsS3BucketBucketLifecycleConfigurationRulesDetails_status :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesDetails_status = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {status} -> status) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {status = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | The unique identifier of the rule.
awsS3BucketBucketLifecycleConfigurationRulesDetails_id :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesDetails_id = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {id} -> id) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {id = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | Identifies the objects that a rule applies to.
awsS3BucketBucketLifecycleConfigurationRulesDetails_filter :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails)
awsS3BucketBucketLifecycleConfigurationRulesDetails_filter = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {filter'} -> filter') (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {filter' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | The length in days of the lifetime for objects that are subject to the
-- rule.
awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Int)
awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationInDays = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {expirationInDays} -> expirationInDays) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {expirationInDays = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | Transition rules that describe when noncurrent objects transition to a
-- specified storage class.
awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesNoncurrentVersionTransitionsDetails])
awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionTransitions = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {noncurrentVersionTransitions} -> noncurrentVersionTransitions) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {noncurrentVersionTransitions = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails) Prelude.. Lens.mapping Lens.coerced

-- | How Amazon S3 responds when a multipart upload is incomplete.
-- Specifically, provides a number of days before Amazon S3 cancels the
-- entire upload.
awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesAbortIncompleteMultipartUploadDetails)
awsS3BucketBucketLifecycleConfigurationRulesDetails_abortIncompleteMultipartUpload = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {abortIncompleteMultipartUpload} -> abortIncompleteMultipartUpload) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {abortIncompleteMultipartUpload = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | A prefix that identifies one or more objects that the rule applies to.
awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | The date when objects are moved or deleted.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesDetails_expirationDate = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {expirationDate} -> expirationDate) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {expirationDate = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

-- | The number of days that an object is noncurrent before Amazon S3 can
-- perform the associated action.
awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesDetails (Prelude.Maybe Prelude.Int)
awsS3BucketBucketLifecycleConfigurationRulesDetails_noncurrentVersionExpirationInDays = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesDetails' {noncurrentVersionExpirationInDays} -> noncurrentVersionExpirationInDays) (\s@AwsS3BucketBucketLifecycleConfigurationRulesDetails' {} a -> s {noncurrentVersionExpirationInDays = a} :: AwsS3BucketBucketLifecycleConfigurationRulesDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesDetails'
            Prelude.<$> (x Core..:? "Transitions" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "ExpiredObjectDeleteMarker")
              Prelude.<*> (x Core..:? "Status")
              Prelude.<*> (x Core..:? "ID")
              Prelude.<*> (x Core..:? "Filter")
              Prelude.<*> (x Core..:? "ExpirationInDays")
              Prelude.<*> ( x Core..:? "NoncurrentVersionTransitions"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "AbortIncompleteMultipartUpload")
              Prelude.<*> (x Core..:? "Prefix")
              Prelude.<*> (x Core..:? "ExpirationDate")
              Prelude.<*> (x Core..:? "NoncurrentVersionExpirationInDays")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesDetails' {..} =
      _salt `Prelude.hashWithSalt` transitions
        `Prelude.hashWithSalt` expiredObjectDeleteMarker
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` expirationInDays
        `Prelude.hashWithSalt` noncurrentVersionTransitions
        `Prelude.hashWithSalt` abortIncompleteMultipartUpload
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` expirationDate
        `Prelude.hashWithSalt` noncurrentVersionExpirationInDays

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesDetails' {..} =
      Prelude.rnf transitions
        `Prelude.seq` Prelude.rnf expiredObjectDeleteMarker
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf id
        `Prelude.seq` Prelude.rnf filter'
        `Prelude.seq` Prelude.rnf expirationInDays
        `Prelude.seq` Prelude.rnf noncurrentVersionTransitions
        `Prelude.seq` Prelude.rnf abortIncompleteMultipartUpload
        `Prelude.seq` Prelude.rnf prefix
        `Prelude.seq` Prelude.rnf expirationDate
        `Prelude.seq` Prelude.rnf noncurrentVersionExpirationInDays

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Transitions" Core..=) Prelude.<$> transitions,
              ("ExpiredObjectDeleteMarker" Core..=)
                Prelude.<$> expiredObjectDeleteMarker,
              ("Status" Core..=) Prelude.<$> status,
              ("ID" Core..=) Prelude.<$> id,
              ("Filter" Core..=) Prelude.<$> filter',
              ("ExpirationInDays" Core..=)
                Prelude.<$> expirationInDays,
              ("NoncurrentVersionTransitions" Core..=)
                Prelude.<$> noncurrentVersionTransitions,
              ("AbortIncompleteMultipartUpload" Core..=)
                Prelude.<$> abortIncompleteMultipartUpload,
              ("Prefix" Core..=) Prelude.<$> prefix,
              ("ExpirationDate" Core..=)
                Prelude.<$> expirationDate,
              ("NoncurrentVersionExpirationInDays" Core..=)
                Prelude.<$> noncurrentVersionExpirationInDays
            ]
        )
