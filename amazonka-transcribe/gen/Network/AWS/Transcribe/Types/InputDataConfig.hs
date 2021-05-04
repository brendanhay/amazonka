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
-- Module      : Network.AWS.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.InputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The object that contains the Amazon S3 object location and access role
-- required to train and tune your custom language model.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The Amazon S3 prefix you specify to access the plain text files that you
    -- use to tune your custom language model.
    tuningDataS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 prefix you specify to access the plain text files that you
    -- use to train your custom language model.
    s3Uri :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the permissions
    -- you\'ve given Amazon Transcribe to access your Amazon S3 buckets
    -- containing your media files or text data.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  InputDataConfig
newInputDataConfig pS3Uri_ pDataAccessRoleArn_ =
  InputDataConfig'
    { tuningDataS3Uri = Prelude.Nothing,
      s3Uri = pS3Uri_,
      dataAccessRoleArn = pDataAccessRoleArn_
    }

-- | The Amazon S3 prefix you specify to access the plain text files that you
-- use to tune your custom language model.
inputDataConfig_tuningDataS3Uri :: Lens.Lens' InputDataConfig (Prelude.Maybe Prelude.Text)
inputDataConfig_tuningDataS3Uri = Lens.lens (\InputDataConfig' {tuningDataS3Uri} -> tuningDataS3Uri) (\s@InputDataConfig' {} a -> s {tuningDataS3Uri = a} :: InputDataConfig)

-- | The Amazon S3 prefix you specify to access the plain text files that you
-- use to train your custom language model.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

-- | The Amazon Resource Name (ARN) that uniquely identifies the permissions
-- you\'ve given Amazon Transcribe to access your Amazon S3 buckets
-- containing your media files or text data.
inputDataConfig_dataAccessRoleArn :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_dataAccessRoleArn = Lens.lens (\InputDataConfig' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@InputDataConfig' {} a -> s {dataAccessRoleArn = a} :: InputDataConfig)

instance Prelude.FromJSON InputDataConfig where
  parseJSON =
    Prelude.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Prelude..:? "TuningDataS3Uri")
            Prelude.<*> (x Prelude..: "S3Uri")
            Prelude.<*> (x Prelude..: "DataAccessRoleArn")
      )

instance Prelude.Hashable InputDataConfig

instance Prelude.NFData InputDataConfig

instance Prelude.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TuningDataS3Uri" Prelude..=)
              Prelude.<$> tuningDataS3Uri,
            Prelude.Just ("S3Uri" Prelude..= s3Uri),
            Prelude.Just
              ("DataAccessRoleArn" Prelude..= dataAccessRoleArn)
          ]
      )
