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
-- Module      : Amazonka.Rekognition.Types.S3Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 bucket location to which Amazon Rekognition publishes the
-- detailed inference results of a video analysis operation. These results
-- include the name of the stream processor resource, the session ID of the
-- stream processing session, and labeled timestamps and bounding boxes for
-- detected labels.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The name of the Amazon S3 bucket you want to associate with the
    -- streaming video project. You must be the owner of the Amazon S3 bucket.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix value of the location within the bucket that you want the
    -- information to be published to. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Using prefixes>.
    keyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Destination_bucket' - The name of the Amazon S3 bucket you want to associate with the
-- streaming video project. You must be the owner of the Amazon S3 bucket.
--
-- 'keyPrefix', 's3Destination_keyPrefix' - The prefix value of the location within the bucket that you want the
-- information to be published to. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Using prefixes>.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { bucket = Prelude.Nothing,
      keyPrefix = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket you want to associate with the
-- streaming video project. You must be the owner of the Amazon S3 bucket.
s3Destination_bucket :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucket = Lens.lens (\S3Destination' {bucket} -> bucket) (\s@S3Destination' {} a -> s {bucket = a} :: S3Destination)

-- | The prefix value of the location within the bucket that you want the
-- information to be published to. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Using prefixes>.
s3Destination_keyPrefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_keyPrefix = Lens.lens (\S3Destination' {keyPrefix} -> keyPrefix) (\s@S3Destination' {} a -> s {keyPrefix = a} :: S3Destination)

instance Data.FromJSON S3Destination where
  parseJSON =
    Data.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Data..:? "Bucket")
            Prelude.<*> (x Data..:? "KeyPrefix")
      )

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` keyPrefix

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf keyPrefix

instance Data.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bucket" Data..=) Prelude.<$> bucket,
            ("KeyPrefix" Data..=) Prelude.<$> keyPrefix
          ]
      )
