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
-- Module      : Amazonka.Forecast.Types.S3Config
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.S3Config where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The path to the file(s) in an Amazon Simple Storage Service (Amazon S3)
-- bucket, and an AWS Identity and Access Management (IAM) role that Amazon
-- Forecast can assume to access the file(s). Optionally, includes an AWS
-- Key Management Service (KMS) key. This object is part of the DataSource
-- object that is submitted in the CreateDatasetImportJob request, and part
-- of the DataDestination object.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
    -- key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The path to an Amazon Simple Storage Service (Amazon S3) bucket or
    -- file(s) in an Amazon S3 bucket.
    path :: Prelude.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
    -- Forecast can assume to access the Amazon S3 bucket or files. If you
    -- provide a value for the @KMSKeyArn@ key, the role must allow access to
    -- the key.
    --
    -- Passing a role across AWS accounts is not allowed. If you pass a role
    -- that isn\'t in your account, you get an @InvalidInputException@ error.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 's3Config_kmsKeyArn' - The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
-- key.
--
-- 'path', 's3Config_path' - The path to an Amazon Simple Storage Service (Amazon S3) bucket or
-- file(s) in an Amazon S3 bucket.
--
-- 'roleArn', 's3Config_roleArn' - The ARN of the AWS Identity and Access Management (IAM) role that Amazon
-- Forecast can assume to access the Amazon S3 bucket or files. If you
-- provide a value for the @KMSKeyArn@ key, the role must allow access to
-- the key.
--
-- Passing a role across AWS accounts is not allowed. If you pass a role
-- that isn\'t in your account, you get an @InvalidInputException@ error.
newS3Config ::
  -- | 'path'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  S3Config
newS3Config pPath_ pRoleArn_ =
  S3Config'
    { kmsKeyArn = Prelude.Nothing,
      path = pPath_,
      roleArn = pRoleArn_
    }

-- | The Amazon Resource Name (ARN) of an AWS Key Management Service (KMS)
-- key.
s3Config_kmsKeyArn :: Lens.Lens' S3Config (Prelude.Maybe Prelude.Text)
s3Config_kmsKeyArn = Lens.lens (\S3Config' {kmsKeyArn} -> kmsKeyArn) (\s@S3Config' {} a -> s {kmsKeyArn = a} :: S3Config)

-- | The path to an Amazon Simple Storage Service (Amazon S3) bucket or
-- file(s) in an Amazon S3 bucket.
s3Config_path :: Lens.Lens' S3Config Prelude.Text
s3Config_path = Lens.lens (\S3Config' {path} -> path) (\s@S3Config' {} a -> s {path = a} :: S3Config)

-- | The ARN of the AWS Identity and Access Management (IAM) role that Amazon
-- Forecast can assume to access the Amazon S3 bucket or files. If you
-- provide a value for the @KMSKeyArn@ key, the role must allow access to
-- the key.
--
-- Passing a role across AWS accounts is not allowed. If you pass a role
-- that isn\'t in your account, you get an @InvalidInputException@ error.
s3Config_roleArn :: Lens.Lens' S3Config Prelude.Text
s3Config_roleArn = Lens.lens (\S3Config' {roleArn} -> roleArn) (\s@S3Config' {} a -> s {roleArn = a} :: S3Config)

instance Data.FromJSON S3Config where
  parseJSON =
    Data.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Data..:? "KMSKeyArn")
            Prelude.<*> (x Data..: "Path")
            Prelude.<*> (x Data..: "RoleArn")
      )

instance Prelude.Hashable S3Config where
  hashWithSalt _salt S3Config' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData S3Config where
  rnf S3Config' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON S3Config where
  toJSON S3Config' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("Path" Data..= path),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
