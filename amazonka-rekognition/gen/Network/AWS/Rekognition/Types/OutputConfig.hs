{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.OutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The S3 bucket and folder location where training output is placed.
--
-- /See:/ 'newOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The S3 bucket where training output is placed.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix applied to the training output files.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'outputConfig_s3Bucket' - The S3 bucket where training output is placed.
--
-- 's3KeyPrefix', 'outputConfig_s3KeyPrefix' - The prefix applied to the training output files.
newOutputConfig ::
  OutputConfig
newOutputConfig =
  OutputConfig'
    { s3Bucket = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing
    }

-- | The S3 bucket where training output is placed.
outputConfig_s3Bucket :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_s3Bucket = Lens.lens (\OutputConfig' {s3Bucket} -> s3Bucket) (\s@OutputConfig' {} a -> s {s3Bucket = a} :: OutputConfig)

-- | The prefix applied to the training output files.
outputConfig_s3KeyPrefix :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_s3KeyPrefix = Lens.lens (\OutputConfig' {s3KeyPrefix} -> s3KeyPrefix) (\s@OutputConfig' {} a -> s {s3KeyPrefix = a} :: OutputConfig)

instance Prelude.FromJSON OutputConfig where
  parseJSON =
    Prelude.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Prelude.<$> (x Prelude..:? "S3Bucket")
            Prelude.<*> (x Prelude..:? "S3KeyPrefix")
      )

instance Prelude.Hashable OutputConfig

instance Prelude.NFData OutputConfig

instance Prelude.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Prelude..=) Prelude.<$> s3Bucket,
            ("S3KeyPrefix" Prelude..=) Prelude.<$> s3KeyPrefix
          ]
      )
