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
-- Module      : Amazonka.SecurityHub.Types.AwsSqsQueueDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSqsQueueDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Data about a queue.
--
-- /See:/ 'newAwsSqsQueueDetails' smart constructor.
data AwsSqsQueueDetails = AwsSqsQueueDetails'
  { -- | The ID of an Amazon Web Services managed key for Amazon SQS or a custom
    -- KMS key.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the new queue.
    queueName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dead-letter queue to which Amazon SQS moves messages
    -- after the value of @maxReceiveCount@ is exceeded.
    deadLetterTargetArn :: Prelude.Maybe Prelude.Text,
    -- | The length of time, in seconds, for which Amazon SQS can reuse a data
    -- key to encrypt or decrypt messages before calling KMS again.
    kmsDataKeyReusePeriodSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSqsQueueDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyId', 'awsSqsQueueDetails_kmsMasterKeyId' - The ID of an Amazon Web Services managed key for Amazon SQS or a custom
-- KMS key.
--
-- 'queueName', 'awsSqsQueueDetails_queueName' - The name of the new queue.
--
-- 'deadLetterTargetArn', 'awsSqsQueueDetails_deadLetterTargetArn' - The ARN of the dead-letter queue to which Amazon SQS moves messages
-- after the value of @maxReceiveCount@ is exceeded.
--
-- 'kmsDataKeyReusePeriodSeconds', 'awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds' - The length of time, in seconds, for which Amazon SQS can reuse a data
-- key to encrypt or decrypt messages before calling KMS again.
newAwsSqsQueueDetails ::
  AwsSqsQueueDetails
newAwsSqsQueueDetails =
  AwsSqsQueueDetails'
    { kmsMasterKeyId =
        Prelude.Nothing,
      queueName = Prelude.Nothing,
      deadLetterTargetArn = Prelude.Nothing,
      kmsDataKeyReusePeriodSeconds = Prelude.Nothing
    }

-- | The ID of an Amazon Web Services managed key for Amazon SQS or a custom
-- KMS key.
awsSqsQueueDetails_kmsMasterKeyId :: Lens.Lens' AwsSqsQueueDetails (Prelude.Maybe Prelude.Text)
awsSqsQueueDetails_kmsMasterKeyId = Lens.lens (\AwsSqsQueueDetails' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@AwsSqsQueueDetails' {} a -> s {kmsMasterKeyId = a} :: AwsSqsQueueDetails)

-- | The name of the new queue.
awsSqsQueueDetails_queueName :: Lens.Lens' AwsSqsQueueDetails (Prelude.Maybe Prelude.Text)
awsSqsQueueDetails_queueName = Lens.lens (\AwsSqsQueueDetails' {queueName} -> queueName) (\s@AwsSqsQueueDetails' {} a -> s {queueName = a} :: AwsSqsQueueDetails)

-- | The ARN of the dead-letter queue to which Amazon SQS moves messages
-- after the value of @maxReceiveCount@ is exceeded.
awsSqsQueueDetails_deadLetterTargetArn :: Lens.Lens' AwsSqsQueueDetails (Prelude.Maybe Prelude.Text)
awsSqsQueueDetails_deadLetterTargetArn = Lens.lens (\AwsSqsQueueDetails' {deadLetterTargetArn} -> deadLetterTargetArn) (\s@AwsSqsQueueDetails' {} a -> s {deadLetterTargetArn = a} :: AwsSqsQueueDetails)

-- | The length of time, in seconds, for which Amazon SQS can reuse a data
-- key to encrypt or decrypt messages before calling KMS again.
awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds :: Lens.Lens' AwsSqsQueueDetails (Prelude.Maybe Prelude.Int)
awsSqsQueueDetails_kmsDataKeyReusePeriodSeconds = Lens.lens (\AwsSqsQueueDetails' {kmsDataKeyReusePeriodSeconds} -> kmsDataKeyReusePeriodSeconds) (\s@AwsSqsQueueDetails' {} a -> s {kmsDataKeyReusePeriodSeconds = a} :: AwsSqsQueueDetails)

instance Core.FromJSON AwsSqsQueueDetails where
  parseJSON =
    Core.withObject
      "AwsSqsQueueDetails"
      ( \x ->
          AwsSqsQueueDetails'
            Prelude.<$> (x Core..:? "KmsMasterKeyId")
            Prelude.<*> (x Core..:? "QueueName")
            Prelude.<*> (x Core..:? "DeadLetterTargetArn")
            Prelude.<*> (x Core..:? "KmsDataKeyReusePeriodSeconds")
      )

instance Prelude.Hashable AwsSqsQueueDetails where
  hashWithSalt _salt AwsSqsQueueDetails' {..} =
    _salt `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` queueName
      `Prelude.hashWithSalt` deadLetterTargetArn
      `Prelude.hashWithSalt` kmsDataKeyReusePeriodSeconds

instance Prelude.NFData AwsSqsQueueDetails where
  rnf AwsSqsQueueDetails' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf queueName
      `Prelude.seq` Prelude.rnf deadLetterTargetArn
      `Prelude.seq` Prelude.rnf kmsDataKeyReusePeriodSeconds

instance Core.ToJSON AwsSqsQueueDetails where
  toJSON AwsSqsQueueDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("QueueName" Core..=) Prelude.<$> queueName,
            ("DeadLetterTargetArn" Core..=)
              Prelude.<$> deadLetterTargetArn,
            ("KmsDataKeyReusePeriodSeconds" Core..=)
              Prelude.<$> kmsDataKeyReusePeriodSeconds
          ]
      )
