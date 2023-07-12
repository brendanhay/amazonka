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
-- Module      : Amazonka.LexV2Models.Types.TranscriptSourceSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TranscriptSourceSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.S3BucketTranscriptSource
import qualified Amazonka.Prelude as Prelude

-- | Indicates the setting of the location where the transcript is stored.
--
-- /See:/ 'newTranscriptSourceSetting' smart constructor.
data TranscriptSourceSetting = TranscriptSourceSetting'
  { -- | Indicates the setting of the Amazon S3 bucket where the transcript is
    -- stored.
    s3BucketTranscriptSource :: Prelude.Maybe S3BucketTranscriptSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptSourceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketTranscriptSource', 'transcriptSourceSetting_s3BucketTranscriptSource' - Indicates the setting of the Amazon S3 bucket where the transcript is
-- stored.
newTranscriptSourceSetting ::
  TranscriptSourceSetting
newTranscriptSourceSetting =
  TranscriptSourceSetting'
    { s3BucketTranscriptSource =
        Prelude.Nothing
    }

-- | Indicates the setting of the Amazon S3 bucket where the transcript is
-- stored.
transcriptSourceSetting_s3BucketTranscriptSource :: Lens.Lens' TranscriptSourceSetting (Prelude.Maybe S3BucketTranscriptSource)
transcriptSourceSetting_s3BucketTranscriptSource = Lens.lens (\TranscriptSourceSetting' {s3BucketTranscriptSource} -> s3BucketTranscriptSource) (\s@TranscriptSourceSetting' {} a -> s {s3BucketTranscriptSource = a} :: TranscriptSourceSetting)

instance Data.FromJSON TranscriptSourceSetting where
  parseJSON =
    Data.withObject
      "TranscriptSourceSetting"
      ( \x ->
          TranscriptSourceSetting'
            Prelude.<$> (x Data..:? "s3BucketTranscriptSource")
      )

instance Prelude.Hashable TranscriptSourceSetting where
  hashWithSalt _salt TranscriptSourceSetting' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketTranscriptSource

instance Prelude.NFData TranscriptSourceSetting where
  rnf TranscriptSourceSetting' {..} =
    Prelude.rnf s3BucketTranscriptSource

instance Data.ToJSON TranscriptSourceSetting where
  toJSON TranscriptSourceSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3BucketTranscriptSource" Data..=)
              Prelude.<$> s3BucketTranscriptSource
          ]
      )
