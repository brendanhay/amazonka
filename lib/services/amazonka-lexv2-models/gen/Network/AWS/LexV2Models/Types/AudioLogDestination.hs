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
-- Module      : Network.AWS.LexV2Models.Types.AudioLogDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.AudioLogDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.S3BucketLogDestination
import qualified Network.AWS.Prelude as Prelude

-- | The location of audio log files collected when conversation logging is
-- enabled for a bot.
--
-- /See:/ 'newAudioLogDestination' smart constructor.
data AudioLogDestination = AudioLogDestination'
  { -- | The Amazon S3 bucket where the audio log files are stored. The IAM role
    -- specified in the @roleArn@ parameter of the CreateBot operation must
    -- have permission to write to this bucket.
    s3Bucket :: S3BucketLogDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioLogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'audioLogDestination_s3Bucket' - The Amazon S3 bucket where the audio log files are stored. The IAM role
-- specified in the @roleArn@ parameter of the CreateBot operation must
-- have permission to write to this bucket.
newAudioLogDestination ::
  -- | 's3Bucket'
  S3BucketLogDestination ->
  AudioLogDestination
newAudioLogDestination pS3Bucket_ =
  AudioLogDestination' {s3Bucket = pS3Bucket_}

-- | The Amazon S3 bucket where the audio log files are stored. The IAM role
-- specified in the @roleArn@ parameter of the CreateBot operation must
-- have permission to write to this bucket.
audioLogDestination_s3Bucket :: Lens.Lens' AudioLogDestination S3BucketLogDestination
audioLogDestination_s3Bucket = Lens.lens (\AudioLogDestination' {s3Bucket} -> s3Bucket) (\s@AudioLogDestination' {} a -> s {s3Bucket = a} :: AudioLogDestination)

instance Core.FromJSON AudioLogDestination where
  parseJSON =
    Core.withObject
      "AudioLogDestination"
      ( \x ->
          AudioLogDestination'
            Prelude.<$> (x Core..: "s3Bucket")
      )

instance Prelude.Hashable AudioLogDestination

instance Prelude.NFData AudioLogDestination

instance Core.ToJSON AudioLogDestination where
  toJSON AudioLogDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3Bucket" Core..= s3Bucket)]
      )
