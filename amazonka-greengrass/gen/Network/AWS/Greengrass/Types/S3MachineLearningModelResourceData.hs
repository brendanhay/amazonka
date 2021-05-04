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
-- Module      : Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData where

import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 'newS3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { ownerSetting :: Prelude.Maybe ResourceDownloadOwnerSetting,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    -- | The URI of the source model in an S3 bucket. The model package must be
    -- in tar.gz or .zip format.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      destinationPath = Prelude.Nothing,
      s3Uri = Prelude.Nothing
    }

-- | Undocumented member.
s3MachineLearningModelResourceData_ownerSetting :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe ResourceDownloadOwnerSetting)
s3MachineLearningModelResourceData_ownerSetting = Lens.lens (\S3MachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@S3MachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: S3MachineLearningModelResourceData)

-- | The absolute local path of the resource inside the Lambda environment.
s3MachineLearningModelResourceData_destinationPath :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
s3MachineLearningModelResourceData_destinationPath = Lens.lens (\S3MachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@S3MachineLearningModelResourceData' {} a -> s {destinationPath = a} :: S3MachineLearningModelResourceData)

-- | The URI of the source model in an S3 bucket. The model package must be
-- in tar.gz or .zip format.
s3MachineLearningModelResourceData_s3Uri :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
s3MachineLearningModelResourceData_s3Uri = Lens.lens (\S3MachineLearningModelResourceData' {s3Uri} -> s3Uri) (\s@S3MachineLearningModelResourceData' {} a -> s {s3Uri = a} :: S3MachineLearningModelResourceData)

instance
  Prelude.FromJSON
    S3MachineLearningModelResourceData
  where
  parseJSON =
    Prelude.withObject
      "S3MachineLearningModelResourceData"
      ( \x ->
          S3MachineLearningModelResourceData'
            Prelude.<$> (x Prelude..:? "OwnerSetting")
            Prelude.<*> (x Prelude..:? "DestinationPath")
            Prelude.<*> (x Prelude..:? "S3Uri")
      )

instance
  Prelude.Hashable
    S3MachineLearningModelResourceData

instance
  Prelude.NFData
    S3MachineLearningModelResourceData

instance
  Prelude.ToJSON
    S3MachineLearningModelResourceData
  where
  toJSON S3MachineLearningModelResourceData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OwnerSetting" Prelude..=)
              Prelude.<$> ownerSetting,
            ("DestinationPath" Prelude..=)
              Prelude.<$> destinationPath,
            ("S3Uri" Prelude..=) Prelude.<$> s3Uri
          ]
      )
