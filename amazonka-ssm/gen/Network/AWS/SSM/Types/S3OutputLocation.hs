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
-- Module      : Network.AWS.SSM.Types.S3OutputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'newS3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { -- | The name of the S3 bucket.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3OutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3BucketName', 's3OutputLocation_outputS3BucketName' - The name of the S3 bucket.
--
-- 'outputS3Region', 's3OutputLocation_outputS3Region' - (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
--
-- 'outputS3KeyPrefix', 's3OutputLocation_outputS3KeyPrefix' - The S3 bucket subfolder.
newS3OutputLocation ::
  S3OutputLocation
newS3OutputLocation =
  S3OutputLocation'
    { outputS3BucketName =
        Prelude.Nothing,
      outputS3Region = Prelude.Nothing,
      outputS3KeyPrefix = Prelude.Nothing
    }

-- | The name of the S3 bucket.
s3OutputLocation_outputS3BucketName :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3BucketName = Lens.lens (\S3OutputLocation' {outputS3BucketName} -> outputS3BucketName) (\s@S3OutputLocation' {} a -> s {outputS3BucketName = a} :: S3OutputLocation)

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
s3OutputLocation_outputS3Region :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3Region = Lens.lens (\S3OutputLocation' {outputS3Region} -> outputS3Region) (\s@S3OutputLocation' {} a -> s {outputS3Region = a} :: S3OutputLocation)

-- | The S3 bucket subfolder.
s3OutputLocation_outputS3KeyPrefix :: Lens.Lens' S3OutputLocation (Prelude.Maybe Prelude.Text)
s3OutputLocation_outputS3KeyPrefix = Lens.lens (\S3OutputLocation' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@S3OutputLocation' {} a -> s {outputS3KeyPrefix = a} :: S3OutputLocation)

instance Prelude.FromJSON S3OutputLocation where
  parseJSON =
    Prelude.withObject
      "S3OutputLocation"
      ( \x ->
          S3OutputLocation'
            Prelude.<$> (x Prelude..:? "OutputS3BucketName")
            Prelude.<*> (x Prelude..:? "OutputS3Region")
            Prelude.<*> (x Prelude..:? "OutputS3KeyPrefix")
      )

instance Prelude.Hashable S3OutputLocation

instance Prelude.NFData S3OutputLocation

instance Prelude.ToJSON S3OutputLocation where
  toJSON S3OutputLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OutputS3BucketName" Prelude..=)
              Prelude.<$> outputS3BucketName,
            ("OutputS3Region" Prelude..=)
              Prelude.<$> outputS3Region,
            ("OutputS3KeyPrefix" Prelude..=)
              Prelude.<$> outputS3KeyPrefix
          ]
      )
