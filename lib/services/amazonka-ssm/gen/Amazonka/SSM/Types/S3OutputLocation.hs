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
-- Module      : Amazonka.SSM.Types.S3OutputLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.S3OutputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'newS3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { -- | The Amazon Web Services Region of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3OutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3Region', 's3OutputLocation_outputS3Region' - The Amazon Web Services Region of the S3 bucket.
--
-- 'outputS3BucketName', 's3OutputLocation_outputS3BucketName' - The name of the S3 bucket.
--
-- 'outputS3KeyPrefix', 's3OutputLocation_outputS3KeyPrefix' - The S3 bucket subfolder.
newS3OutputLocation ::
  S3OutputLocation
newS3OutputLocation =
  S3OutputLocation'
    { outputS3Region = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing
    }

-- | The Amazon Web Services Region of the S3 bucket.
s3OutputLocation_outputS3Region :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3Region = Lens.lens (\S3OutputLocation' {outputS3Region} -> outputS3Region) (\s@S3OutputLocation' {} a -> s {outputS3Region = a} :: S3OutputLocation)

-- | The name of the S3 bucket.
s3OutputLocation_outputS3BucketName :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3BucketName = Lens.lens (\S3OutputLocation' {outputS3BucketName} -> outputS3BucketName) (\s@S3OutputLocation' {} a -> s {outputS3BucketName = a} :: S3OutputLocation)

-- | The S3 bucket subfolder.
s3OutputLocation_outputS3KeyPrefix :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3KeyPrefix = Lens.lens (\S3OutputLocation' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@S3OutputLocation' {} a -> s {outputS3KeyPrefix = a} :: S3OutputLocation)

instance Core.FromJSON S3OutputLocation where
  parseJSON =
    Core.withObject
      "S3OutputLocation"
      ( \x ->
          S3OutputLocation'
            Prelude.<$> (x Core..:? "OutputS3Region")
            Prelude.<*> (x Core..:? "OutputS3BucketName")
            Prelude.<*> (x Core..:? "OutputS3KeyPrefix")
      )

instance Prelude.Hashable S3OutputLocation where
  hashWithSalt _salt S3OutputLocation' {..} =
    _salt `Prelude.hashWithSalt` outputS3Region
      `Prelude.hashWithSalt` outputS3BucketName
      `Prelude.hashWithSalt` outputS3KeyPrefix

instance Prelude.NFData S3OutputLocation where
  rnf S3OutputLocation' {..} =
    Prelude.rnf outputS3Region
      `Prelude.seq` Prelude.rnf outputS3BucketName
      `Prelude.seq` Prelude.rnf outputS3KeyPrefix

instance Core.ToJSON S3OutputLocation where
  toJSON S3OutputLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutputS3Region" Core..=)
              Prelude.<$> outputS3Region,
            ("OutputS3BucketName" Core..=)
              Prelude.<$> outputS3BucketName,
            ("OutputS3KeyPrefix" Core..=)
              Prelude.<$> outputS3KeyPrefix
          ]
      )
