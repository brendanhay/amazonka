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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'newS3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { -- | The name of the S3 bucket.
    outputS3BucketName :: Core.Maybe Core.Text,
    -- | (Deprecated) You can no longer specify this parameter. The system
    -- ignores it. Instead, Systems Manager automatically determines the Region
    -- of the S3 bucket.
    outputS3Region :: Core.Maybe Core.Text,
    -- | The S3 bucket subfolder.
    outputS3KeyPrefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      outputS3Region = Core.Nothing,
      outputS3KeyPrefix = Core.Nothing
    }

-- | The name of the S3 bucket.
s3OutputLocation_outputS3BucketName :: Lens.Lens' S3OutputLocation (Core.Maybe Core.Text)
s3OutputLocation_outputS3BucketName = Lens.lens (\S3OutputLocation' {outputS3BucketName} -> outputS3BucketName) (\s@S3OutputLocation' {} a -> s {outputS3BucketName = a} :: S3OutputLocation)

-- | (Deprecated) You can no longer specify this parameter. The system
-- ignores it. Instead, Systems Manager automatically determines the Region
-- of the S3 bucket.
s3OutputLocation_outputS3Region :: Lens.Lens' S3OutputLocation (Core.Maybe Core.Text)
s3OutputLocation_outputS3Region = Lens.lens (\S3OutputLocation' {outputS3Region} -> outputS3Region) (\s@S3OutputLocation' {} a -> s {outputS3Region = a} :: S3OutputLocation)

-- | The S3 bucket subfolder.
s3OutputLocation_outputS3KeyPrefix :: Lens.Lens' S3OutputLocation (Core.Maybe Core.Text)
s3OutputLocation_outputS3KeyPrefix = Lens.lens (\S3OutputLocation' {outputS3KeyPrefix} -> outputS3KeyPrefix) (\s@S3OutputLocation' {} a -> s {outputS3KeyPrefix = a} :: S3OutputLocation)

instance Core.FromJSON S3OutputLocation where
  parseJSON =
    Core.withObject
      "S3OutputLocation"
      ( \x ->
          S3OutputLocation'
            Core.<$> (x Core..:? "OutputS3BucketName")
            Core.<*> (x Core..:? "OutputS3Region")
            Core.<*> (x Core..:? "OutputS3KeyPrefix")
      )

instance Core.Hashable S3OutputLocation

instance Core.NFData S3OutputLocation

instance Core.ToJSON S3OutputLocation where
  toJSON S3OutputLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutputS3BucketName" Core..=)
              Core.<$> outputS3BucketName,
            ("OutputS3Region" Core..=) Core.<$> outputS3Region,
            ("OutputS3KeyPrefix" Core..=)
              Core.<$> outputS3KeyPrefix
          ]
      )
