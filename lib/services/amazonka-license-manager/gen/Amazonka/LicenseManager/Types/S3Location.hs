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
-- Module      : Amazonka.LicenseManager.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the S3 bucket that report generator reports are published to.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | Name of the S3 bucket reports are published to.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | Prefix of the S3 bucket reports are published to.
    keyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Location_bucket' - Name of the S3 bucket reports are published to.
--
-- 'keyPrefix', 's3Location_keyPrefix' - Prefix of the S3 bucket reports are published to.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { bucket = Prelude.Nothing,
      keyPrefix = Prelude.Nothing
    }

-- | Name of the S3 bucket reports are published to.
s3Location_bucket :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

-- | Prefix of the S3 bucket reports are published to.
s3Location_keyPrefix :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_keyPrefix = Lens.lens (\S3Location' {keyPrefix} -> keyPrefix) (\s@S3Location' {} a -> s {keyPrefix = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "keyPrefix")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` keyPrefix

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf keyPrefix
