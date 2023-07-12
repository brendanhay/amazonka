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
-- Module      : Amazonka.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.InputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Amazon S3 location of the training data you want to use to
-- create a new custom language model, and permissions to access this
-- location.
--
-- When using @InputDataConfig@, you must include these sub-parameters:
-- @S3Uri@ and @DataAccessRoleArn@. You can optionally include
-- @TuningDataS3Uri@.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The Amazon S3 location (URI) of the text files you want to use to tune
    -- your custom language model.
    --
    -- Here\'s an example URI path:
    -- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-tuning-data\/@
    tuningDataS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location (URI) of the text files you want to use to train
    -- your custom language model.
    --
    -- Here\'s an example URI path:
    -- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-training-data\/@
    s3Uri :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
    -- access the Amazon S3 bucket that contains your input files. If the role
    -- that you specify doesn’t have the appropriate permissions to access the
    -- specified Amazon S3 location, your request fails.
    --
    -- IAM role ARNs have the format
    -- @arn:partition:iam::account:role\/role-name-with-path@. For example:
    -- @arn:aws:iam::111122223333:role\/Admin@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
    dataAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tuningDataS3Uri', 'inputDataConfig_tuningDataS3Uri' - The Amazon S3 location (URI) of the text files you want to use to tune
-- your custom language model.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-tuning-data\/@
--
-- 's3Uri', 'inputDataConfig_s3Uri' - The Amazon S3 location (URI) of the text files you want to use to train
-- your custom language model.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-training-data\/@
--
-- 'dataAccessRoleArn', 'inputDataConfig_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- that you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
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

-- | The Amazon S3 location (URI) of the text files you want to use to tune
-- your custom language model.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-tuning-data\/@
inputDataConfig_tuningDataS3Uri :: Lens.Lens' InputDataConfig (Prelude.Maybe Prelude.Text)
inputDataConfig_tuningDataS3Uri = Lens.lens (\InputDataConfig' {tuningDataS3Uri} -> tuningDataS3Uri) (\s@InputDataConfig' {} a -> s {tuningDataS3Uri = a} :: InputDataConfig)

-- | The Amazon S3 location (URI) of the text files you want to use to train
-- your custom language model.
--
-- Here\'s an example URI path:
-- @s3:\/\/DOC-EXAMPLE-BUCKET\/my-model-training-data\/@
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

-- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the Amazon S3 bucket that contains your input files. If the role
-- that you specify doesn’t have the appropriate permissions to access the
-- specified Amazon S3 location, your request fails.
--
-- IAM role ARNs have the format
-- @arn:partition:iam::account:role\/role-name-with-path@. For example:
-- @arn:aws:iam::111122223333:role\/Admin@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM ARNs>.
inputDataConfig_dataAccessRoleArn :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_dataAccessRoleArn = Lens.lens (\InputDataConfig' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@InputDataConfig' {} a -> s {dataAccessRoleArn = a} :: InputDataConfig)

instance Data.FromJSON InputDataConfig where
  parseJSON =
    Data.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Data..:? "TuningDataS3Uri")
            Prelude.<*> (x Data..: "S3Uri")
            Prelude.<*> (x Data..: "DataAccessRoleArn")
      )

instance Prelude.Hashable InputDataConfig where
  hashWithSalt _salt InputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` tuningDataS3Uri
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData InputDataConfig where
  rnf InputDataConfig' {..} =
    Prelude.rnf tuningDataS3Uri
      `Prelude.seq` Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf dataAccessRoleArn

instance Data.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TuningDataS3Uri" Data..=)
              Prelude.<$> tuningDataS3Uri,
            Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn)
          ]
      )
