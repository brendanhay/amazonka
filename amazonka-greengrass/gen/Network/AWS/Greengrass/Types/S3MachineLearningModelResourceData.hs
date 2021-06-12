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
-- Module      : Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 'newS3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { ownerSetting :: Core.Maybe ResourceDownloadOwnerSetting,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Core.Maybe Core.Text,
    -- | The URI of the source model in an S3 bucket. The model package must be
    -- in tar.gz or .zip format.
    s3Uri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3MachineLearningModelResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerSetting', 's3MachineLearningModelResourceData_ownerSetting' - Undocumented member.
--
-- 'destinationPath', 's3MachineLearningModelResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 's3Uri', 's3MachineLearningModelResourceData_s3Uri' - The URI of the source model in an S3 bucket. The model package must be
-- in tar.gz or .zip format.
newS3MachineLearningModelResourceData ::
  S3MachineLearningModelResourceData
newS3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    { ownerSetting =
        Core.Nothing,
      destinationPath = Core.Nothing,
      s3Uri = Core.Nothing
    }

-- | Undocumented member.
s3MachineLearningModelResourceData_ownerSetting :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe ResourceDownloadOwnerSetting)
s3MachineLearningModelResourceData_ownerSetting = Lens.lens (\S3MachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@S3MachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: S3MachineLearningModelResourceData)

-- | The absolute local path of the resource inside the Lambda environment.
s3MachineLearningModelResourceData_destinationPath :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe Core.Text)
s3MachineLearningModelResourceData_destinationPath = Lens.lens (\S3MachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@S3MachineLearningModelResourceData' {} a -> s {destinationPath = a} :: S3MachineLearningModelResourceData)

-- | The URI of the source model in an S3 bucket. The model package must be
-- in tar.gz or .zip format.
s3MachineLearningModelResourceData_s3Uri :: Lens.Lens' S3MachineLearningModelResourceData (Core.Maybe Core.Text)
s3MachineLearningModelResourceData_s3Uri = Lens.lens (\S3MachineLearningModelResourceData' {s3Uri} -> s3Uri) (\s@S3MachineLearningModelResourceData' {} a -> s {s3Uri = a} :: S3MachineLearningModelResourceData)

instance
  Core.FromJSON
    S3MachineLearningModelResourceData
  where
  parseJSON =
    Core.withObject
      "S3MachineLearningModelResourceData"
      ( \x ->
          S3MachineLearningModelResourceData'
            Core.<$> (x Core..:? "OwnerSetting")
            Core.<*> (x Core..:? "DestinationPath")
            Core.<*> (x Core..:? "S3Uri")
      )

instance
  Core.Hashable
    S3MachineLearningModelResourceData

instance
  Core.NFData
    S3MachineLearningModelResourceData

instance
  Core.ToJSON
    S3MachineLearningModelResourceData
  where
  toJSON S3MachineLearningModelResourceData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OwnerSetting" Core..=) Core.<$> ownerSetting,
            ("DestinationPath" Core..=) Core.<$> destinationPath,
            ("S3Uri" Core..=) Core.<$> s3Uri
          ]
      )
