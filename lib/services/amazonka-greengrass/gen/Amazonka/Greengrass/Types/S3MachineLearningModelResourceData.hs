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
-- Module      : Amazonka.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.S3MachineLearningModelResourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 'newS3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Prelude.Maybe Prelude.Text,
    ownerSetting :: Prelude.Maybe ResourceDownloadOwnerSetting,
    -- | The URI of the source model in an S3 bucket. The model package must be
    -- in tar.gz or .zip format.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3MachineLearningModelResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPath', 's3MachineLearningModelResourceData_destinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- 'ownerSetting', 's3MachineLearningModelResourceData_ownerSetting' - Undocumented member.
--
-- 's3Uri', 's3MachineLearningModelResourceData_s3Uri' - The URI of the source model in an S3 bucket. The model package must be
-- in tar.gz or .zip format.
newS3MachineLearningModelResourceData ::
  S3MachineLearningModelResourceData
newS3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    { destinationPath =
        Prelude.Nothing,
      ownerSetting = Prelude.Nothing,
      s3Uri = Prelude.Nothing
    }

-- | The absolute local path of the resource inside the Lambda environment.
s3MachineLearningModelResourceData_destinationPath :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
s3MachineLearningModelResourceData_destinationPath = Lens.lens (\S3MachineLearningModelResourceData' {destinationPath} -> destinationPath) (\s@S3MachineLearningModelResourceData' {} a -> s {destinationPath = a} :: S3MachineLearningModelResourceData)

-- | Undocumented member.
s3MachineLearningModelResourceData_ownerSetting :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe ResourceDownloadOwnerSetting)
s3MachineLearningModelResourceData_ownerSetting = Lens.lens (\S3MachineLearningModelResourceData' {ownerSetting} -> ownerSetting) (\s@S3MachineLearningModelResourceData' {} a -> s {ownerSetting = a} :: S3MachineLearningModelResourceData)

-- | The URI of the source model in an S3 bucket. The model package must be
-- in tar.gz or .zip format.
s3MachineLearningModelResourceData_s3Uri :: Lens.Lens' S3MachineLearningModelResourceData (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..:? "DestinationPath")
            Prelude.<*> (x Core..:? "OwnerSetting")
            Prelude.<*> (x Core..:? "S3Uri")
      )

instance
  Prelude.Hashable
    S3MachineLearningModelResourceData
  where
  hashWithSalt
    _salt
    S3MachineLearningModelResourceData' {..} =
      _salt `Prelude.hashWithSalt` destinationPath
        `Prelude.hashWithSalt` ownerSetting
        `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    S3MachineLearningModelResourceData
  where
  rnf S3MachineLearningModelResourceData' {..} =
    Prelude.rnf destinationPath
      `Prelude.seq` Prelude.rnf ownerSetting
      `Prelude.seq` Prelude.rnf s3Uri

instance
  Core.ToJSON
    S3MachineLearningModelResourceData
  where
  toJSON S3MachineLearningModelResourceData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DestinationPath" Core..=)
              Prelude.<$> destinationPath,
            ("OwnerSetting" Core..=) Prelude.<$> ownerSetting,
            ("S3Uri" Core..=) Prelude.<$> s3Uri
          ]
      )
