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
-- Module      : Amazonka.Glue.Types.S3Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data store in Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'newS3Target' smart constructor.
data S3Target = S3Target'
  { -- | The name of a connection which allows a job or crawler to access data in
    -- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
    -- VPC).
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A valid Amazon dead-letter SQS ARN. For example,
    -- @arn:aws:sqs:region:account:deadLetterQueue@.
    dlqEventQueueArn :: Prelude.Maybe Prelude.Text,
    -- | A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
    eventQueueArn :: Prelude.Maybe Prelude.Text,
    -- | A list of glob patterns used to exclude from the crawl. For more
    -- information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
    exclusions :: Prelude.Maybe [Prelude.Text],
    -- | The path to the Amazon S3 target.
    path :: Prelude.Maybe Prelude.Text,
    -- | Sets the number of files in each leaf folder to be crawled when crawling
    -- sample files in a dataset. If not set, all the files are crawled. A
    -- valid value is an integer between 1 and 249.
    sampleSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 's3Target_connectionName' - The name of a connection which allows a job or crawler to access data in
-- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
-- VPC).
--
-- 'dlqEventQueueArn', 's3Target_dlqEventQueueArn' - A valid Amazon dead-letter SQS ARN. For example,
-- @arn:aws:sqs:region:account:deadLetterQueue@.
--
-- 'eventQueueArn', 's3Target_eventQueueArn' - A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
--
-- 'exclusions', 's3Target_exclusions' - A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
--
-- 'path', 's3Target_path' - The path to the Amazon S3 target.
--
-- 'sampleSize', 's3Target_sampleSize' - Sets the number of files in each leaf folder to be crawled when crawling
-- sample files in a dataset. If not set, all the files are crawled. A
-- valid value is an integer between 1 and 249.
newS3Target ::
  S3Target
newS3Target =
  S3Target'
    { connectionName = Prelude.Nothing,
      dlqEventQueueArn = Prelude.Nothing,
      eventQueueArn = Prelude.Nothing,
      exclusions = Prelude.Nothing,
      path = Prelude.Nothing,
      sampleSize = Prelude.Nothing
    }

-- | The name of a connection which allows a job or crawler to access data in
-- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
-- VPC).
s3Target_connectionName :: Lens.Lens' S3Target (Prelude.Maybe Prelude.Text)
s3Target_connectionName = Lens.lens (\S3Target' {connectionName} -> connectionName) (\s@S3Target' {} a -> s {connectionName = a} :: S3Target)

-- | A valid Amazon dead-letter SQS ARN. For example,
-- @arn:aws:sqs:region:account:deadLetterQueue@.
s3Target_dlqEventQueueArn :: Lens.Lens' S3Target (Prelude.Maybe Prelude.Text)
s3Target_dlqEventQueueArn = Lens.lens (\S3Target' {dlqEventQueueArn} -> dlqEventQueueArn) (\s@S3Target' {} a -> s {dlqEventQueueArn = a} :: S3Target)

-- | A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
s3Target_eventQueueArn :: Lens.Lens' S3Target (Prelude.Maybe Prelude.Text)
s3Target_eventQueueArn = Lens.lens (\S3Target' {eventQueueArn} -> eventQueueArn) (\s@S3Target' {} a -> s {eventQueueArn = a} :: S3Target)

-- | A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
s3Target_exclusions :: Lens.Lens' S3Target (Prelude.Maybe [Prelude.Text])
s3Target_exclusions = Lens.lens (\S3Target' {exclusions} -> exclusions) (\s@S3Target' {} a -> s {exclusions = a} :: S3Target) Prelude.. Lens.mapping Lens.coerced

-- | The path to the Amazon S3 target.
s3Target_path :: Lens.Lens' S3Target (Prelude.Maybe Prelude.Text)
s3Target_path = Lens.lens (\S3Target' {path} -> path) (\s@S3Target' {} a -> s {path = a} :: S3Target)

-- | Sets the number of files in each leaf folder to be crawled when crawling
-- sample files in a dataset. If not set, all the files are crawled. A
-- valid value is an integer between 1 and 249.
s3Target_sampleSize :: Lens.Lens' S3Target (Prelude.Maybe Prelude.Int)
s3Target_sampleSize = Lens.lens (\S3Target' {sampleSize} -> sampleSize) (\s@S3Target' {} a -> s {sampleSize = a} :: S3Target)

instance Data.FromJSON S3Target where
  parseJSON =
    Data.withObject
      "S3Target"
      ( \x ->
          S3Target'
            Prelude.<$> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "DlqEventQueueArn")
            Prelude.<*> (x Data..:? "EventQueueArn")
            Prelude.<*> (x Data..:? "Exclusions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "SampleSize")
      )

instance Prelude.Hashable S3Target where
  hashWithSalt _salt S3Target' {..} =
    _salt `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` dlqEventQueueArn
      `Prelude.hashWithSalt` eventQueueArn
      `Prelude.hashWithSalt` exclusions
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` sampleSize

instance Prelude.NFData S3Target where
  rnf S3Target' {..} =
    Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf dlqEventQueueArn
      `Prelude.seq` Prelude.rnf eventQueueArn
      `Prelude.seq` Prelude.rnf exclusions
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf sampleSize

instance Data.ToJSON S3Target where
  toJSON S3Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("DlqEventQueueArn" Data..=)
              Prelude.<$> dlqEventQueueArn,
            ("EventQueueArn" Data..=) Prelude.<$> eventQueueArn,
            ("Exclusions" Data..=) Prelude.<$> exclusions,
            ("Path" Data..=) Prelude.<$> path,
            ("SampleSize" Data..=) Prelude.<$> sampleSize
          ]
      )
