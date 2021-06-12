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
-- Module      : Network.AWS.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.InputDataConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The object that contains the Amazon S3 object location and access role
-- required to train and tune your custom language model.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The Amazon S3 prefix you specify to access the plain text files that you
    -- use to tune your custom language model.
    tuningDataS3Uri :: Core.Maybe Core.Text,
    -- | The Amazon S3 prefix you specify to access the plain text files that you
    -- use to train your custom language model.
    s3Uri :: Core.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the permissions
    -- you\'ve given Amazon Transcribe to access your Amazon S3 buckets
    -- containing your media files or text data.
    dataAccessRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tuningDataS3Uri', 'inputDataConfig_tuningDataS3Uri' - The Amazon S3 prefix you specify to access the plain text files that you
-- use to tune your custom language model.
--
-- 's3Uri', 'inputDataConfig_s3Uri' - The Amazon S3 prefix you specify to access the plain text files that you
-- use to train your custom language model.
--
-- 'dataAccessRoleArn', 'inputDataConfig_dataAccessRoleArn' - The Amazon Resource Name (ARN) that uniquely identifies the permissions
-- you\'ve given Amazon Transcribe to access your Amazon S3 buckets
-- containing your media files or text data.
newInputDataConfig ::
  -- | 's3Uri'
  Core.Text ->
  -- | 'dataAccessRoleArn'
  Core.Text ->
  InputDataConfig
newInputDataConfig pS3Uri_ pDataAccessRoleArn_ =
  InputDataConfig'
    { tuningDataS3Uri = Core.Nothing,
      s3Uri = pS3Uri_,
      dataAccessRoleArn = pDataAccessRoleArn_
    }

-- | The Amazon S3 prefix you specify to access the plain text files that you
-- use to tune your custom language model.
inputDataConfig_tuningDataS3Uri :: Lens.Lens' InputDataConfig (Core.Maybe Core.Text)
inputDataConfig_tuningDataS3Uri = Lens.lens (\InputDataConfig' {tuningDataS3Uri} -> tuningDataS3Uri) (\s@InputDataConfig' {} a -> s {tuningDataS3Uri = a} :: InputDataConfig)

-- | The Amazon S3 prefix you specify to access the plain text files that you
-- use to train your custom language model.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Core.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

-- | The Amazon Resource Name (ARN) that uniquely identifies the permissions
-- you\'ve given Amazon Transcribe to access your Amazon S3 buckets
-- containing your media files or text data.
inputDataConfig_dataAccessRoleArn :: Lens.Lens' InputDataConfig Core.Text
inputDataConfig_dataAccessRoleArn = Lens.lens (\InputDataConfig' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@InputDataConfig' {} a -> s {dataAccessRoleArn = a} :: InputDataConfig)

instance Core.FromJSON InputDataConfig where
  parseJSON =
    Core.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Core.<$> (x Core..:? "TuningDataS3Uri")
            Core.<*> (x Core..: "S3Uri")
            Core.<*> (x Core..: "DataAccessRoleArn")
      )

instance Core.Hashable InputDataConfig

instance Core.NFData InputDataConfig

instance Core.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TuningDataS3Uri" Core..=)
              Core.<$> tuningDataS3Uri,
            Core.Just ("S3Uri" Core..= s3Uri),
            Core.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn)
          ]
      )
