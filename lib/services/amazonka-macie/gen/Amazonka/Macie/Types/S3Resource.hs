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
-- Module      : Amazonka.Macie.Types.S3Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.S3Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) Contains information about the S3 resource. This data
-- type is used as a request parameter in the @DisassociateS3Resources@
-- action and can be used as a response parameter in the
-- @AssociateS3Resources@ and @UpdateS3Resources@ actions.
--
-- /See:/ 'newS3Resource' smart constructor.
data S3Resource = S3Resource'
  { -- | (Discontinued) The prefix of the S3 bucket.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The name of the S3 bucket.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3Resource_prefix' - (Discontinued) The prefix of the S3 bucket.
--
-- 'bucketName', 's3Resource_bucketName' - (Discontinued) The name of the S3 bucket.
newS3Resource ::
  -- | 'bucketName'
  Prelude.Text ->
  S3Resource
newS3Resource pBucketName_ =
  S3Resource'
    { prefix = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | (Discontinued) The prefix of the S3 bucket.
s3Resource_prefix :: Lens.Lens' S3Resource (Prelude.Maybe Prelude.Text)
s3Resource_prefix = Lens.lens (\S3Resource' {prefix} -> prefix) (\s@S3Resource' {} a -> s {prefix = a} :: S3Resource)

-- | (Discontinued) The name of the S3 bucket.
s3Resource_bucketName :: Lens.Lens' S3Resource Prelude.Text
s3Resource_bucketName = Lens.lens (\S3Resource' {bucketName} -> bucketName) (\s@S3Resource' {} a -> s {bucketName = a} :: S3Resource)

instance Data.FromJSON S3Resource where
  parseJSON =
    Data.withObject
      "S3Resource"
      ( \x ->
          S3Resource'
            Prelude.<$> (x Data..:? "prefix")
            Prelude.<*> (x Data..: "bucketName")
      )

instance Prelude.Hashable S3Resource where
  hashWithSalt _salt S3Resource' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData S3Resource where
  rnf S3Resource' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON S3Resource where
  toJSON S3Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("bucketName" Data..= bucketName)
          ]
      )
