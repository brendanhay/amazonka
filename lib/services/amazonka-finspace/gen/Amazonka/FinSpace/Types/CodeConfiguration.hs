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
-- Module      : Amazonka.FinSpace.Types.CodeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.CodeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure of the customer code available within the running cluster.
--
-- /See:/ 'newCodeConfiguration' smart constructor.
data CodeConfiguration = CodeConfiguration'
  { -- | A unique name for the S3 bucket.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The full S3 path (excluding bucket) to the .zip file. This file contains
    -- the code that is loaded onto the cluster when it\'s started.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The version of an S3 object.
    s3ObjectVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'codeConfiguration_s3Bucket' - A unique name for the S3 bucket.
--
-- 's3Key', 'codeConfiguration_s3Key' - The full S3 path (excluding bucket) to the .zip file. This file contains
-- the code that is loaded onto the cluster when it\'s started.
--
-- 's3ObjectVersion', 'codeConfiguration_s3ObjectVersion' - The version of an S3 object.
newCodeConfiguration ::
  CodeConfiguration
newCodeConfiguration =
  CodeConfiguration'
    { s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      s3ObjectVersion = Prelude.Nothing
    }

-- | A unique name for the S3 bucket.
codeConfiguration_s3Bucket :: Lens.Lens' CodeConfiguration (Prelude.Maybe Prelude.Text)
codeConfiguration_s3Bucket = Lens.lens (\CodeConfiguration' {s3Bucket} -> s3Bucket) (\s@CodeConfiguration' {} a -> s {s3Bucket = a} :: CodeConfiguration)

-- | The full S3 path (excluding bucket) to the .zip file. This file contains
-- the code that is loaded onto the cluster when it\'s started.
codeConfiguration_s3Key :: Lens.Lens' CodeConfiguration (Prelude.Maybe Prelude.Text)
codeConfiguration_s3Key = Lens.lens (\CodeConfiguration' {s3Key} -> s3Key) (\s@CodeConfiguration' {} a -> s {s3Key = a} :: CodeConfiguration)

-- | The version of an S3 object.
codeConfiguration_s3ObjectVersion :: Lens.Lens' CodeConfiguration (Prelude.Maybe Prelude.Text)
codeConfiguration_s3ObjectVersion = Lens.lens (\CodeConfiguration' {s3ObjectVersion} -> s3ObjectVersion) (\s@CodeConfiguration' {} a -> s {s3ObjectVersion = a} :: CodeConfiguration)

instance Data.FromJSON CodeConfiguration where
  parseJSON =
    Data.withObject
      "CodeConfiguration"
      ( \x ->
          CodeConfiguration'
            Prelude.<$> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3Key")
            Prelude.<*> (x Data..:? "s3ObjectVersion")
      )

instance Prelude.Hashable CodeConfiguration where
  hashWithSalt _salt CodeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3ObjectVersion

instance Prelude.NFData CodeConfiguration where
  rnf CodeConfiguration' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3ObjectVersion

instance Data.ToJSON CodeConfiguration where
  toJSON CodeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("s3Key" Data..=) Prelude.<$> s3Key,
            ("s3ObjectVersion" Data..=)
              Prelude.<$> s3ObjectVersion
          ]
      )
