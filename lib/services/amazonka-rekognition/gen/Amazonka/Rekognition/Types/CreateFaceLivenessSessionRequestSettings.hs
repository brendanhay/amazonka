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
-- Module      : Amazonka.Rekognition.Types.CreateFaceLivenessSessionRequestSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CreateFaceLivenessSessionRequestSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.LivenessOutputConfig

-- | A session settings object. It contains settings for the operation to be
-- performed. It accepts arguments for OutputConfig and AuditImagesLimit.
--
-- /See:/ 'newCreateFaceLivenessSessionRequestSettings' smart constructor.
data CreateFaceLivenessSessionRequestSettings = CreateFaceLivenessSessionRequestSettings'
  { -- | Number of audit images to be returned back. Takes an integer between
    -- 0-4. Any integer less than 0 will return 0, any integer above 4 will
    -- return 4 images in the response. By default, it is set to 0. The limit
    -- is best effort and is based on the actual duration of the selfie-video.
    auditImagesLimit :: Prelude.Maybe Prelude.Natural,
    -- | Can specify the location of an Amazon S3 bucket, where reference and
    -- audit images will be stored. Note that the Amazon S3 bucket must be
    -- located in the caller\'s AWS account and in the same region as the Face
    -- Liveness end-point. Additionally, the Amazon S3 object keys are
    -- auto-generated by the Face Liveness system. Requires that the caller has
    -- the @s3:PutObject@ permission on the Amazon S3 bucket.
    outputConfig :: Prelude.Maybe LivenessOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFaceLivenessSessionRequestSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditImagesLimit', 'createFaceLivenessSessionRequestSettings_auditImagesLimit' - Number of audit images to be returned back. Takes an integer between
-- 0-4. Any integer less than 0 will return 0, any integer above 4 will
-- return 4 images in the response. By default, it is set to 0. The limit
-- is best effort and is based on the actual duration of the selfie-video.
--
-- 'outputConfig', 'createFaceLivenessSessionRequestSettings_outputConfig' - Can specify the location of an Amazon S3 bucket, where reference and
-- audit images will be stored. Note that the Amazon S3 bucket must be
-- located in the caller\'s AWS account and in the same region as the Face
-- Liveness end-point. Additionally, the Amazon S3 object keys are
-- auto-generated by the Face Liveness system. Requires that the caller has
-- the @s3:PutObject@ permission on the Amazon S3 bucket.
newCreateFaceLivenessSessionRequestSettings ::
  CreateFaceLivenessSessionRequestSettings
newCreateFaceLivenessSessionRequestSettings =
  CreateFaceLivenessSessionRequestSettings'
    { auditImagesLimit =
        Prelude.Nothing,
      outputConfig = Prelude.Nothing
    }

-- | Number of audit images to be returned back. Takes an integer between
-- 0-4. Any integer less than 0 will return 0, any integer above 4 will
-- return 4 images in the response. By default, it is set to 0. The limit
-- is best effort and is based on the actual duration of the selfie-video.
createFaceLivenessSessionRequestSettings_auditImagesLimit :: Lens.Lens' CreateFaceLivenessSessionRequestSettings (Prelude.Maybe Prelude.Natural)
createFaceLivenessSessionRequestSettings_auditImagesLimit = Lens.lens (\CreateFaceLivenessSessionRequestSettings' {auditImagesLimit} -> auditImagesLimit) (\s@CreateFaceLivenessSessionRequestSettings' {} a -> s {auditImagesLimit = a} :: CreateFaceLivenessSessionRequestSettings)

-- | Can specify the location of an Amazon S3 bucket, where reference and
-- audit images will be stored. Note that the Amazon S3 bucket must be
-- located in the caller\'s AWS account and in the same region as the Face
-- Liveness end-point. Additionally, the Amazon S3 object keys are
-- auto-generated by the Face Liveness system. Requires that the caller has
-- the @s3:PutObject@ permission on the Amazon S3 bucket.
createFaceLivenessSessionRequestSettings_outputConfig :: Lens.Lens' CreateFaceLivenessSessionRequestSettings (Prelude.Maybe LivenessOutputConfig)
createFaceLivenessSessionRequestSettings_outputConfig = Lens.lens (\CreateFaceLivenessSessionRequestSettings' {outputConfig} -> outputConfig) (\s@CreateFaceLivenessSessionRequestSettings' {} a -> s {outputConfig = a} :: CreateFaceLivenessSessionRequestSettings)

instance
  Prelude.Hashable
    CreateFaceLivenessSessionRequestSettings
  where
  hashWithSalt
    _salt
    CreateFaceLivenessSessionRequestSettings' {..} =
      _salt
        `Prelude.hashWithSalt` auditImagesLimit
        `Prelude.hashWithSalt` outputConfig

instance
  Prelude.NFData
    CreateFaceLivenessSessionRequestSettings
  where
  rnf CreateFaceLivenessSessionRequestSettings' {..} =
    Prelude.rnf auditImagesLimit
      `Prelude.seq` Prelude.rnf outputConfig

instance
  Data.ToJSON
    CreateFaceLivenessSessionRequestSettings
  where
  toJSON CreateFaceLivenessSessionRequestSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditImagesLimit" Data..=)
              Prelude.<$> auditImagesLimit,
            ("OutputConfig" Data..=) Prelude.<$> outputConfig
          ]
      )
