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
-- Module      : Network.AWS.CodePipeline.Types.S3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.S3Location where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon S3 artifact location for an action\'s artifacts.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The artifact name.
    key :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 artifact bucket for an action\'s artifacts.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3Location_key' - The artifact name.
--
-- 'bucket', 's3Location_bucket' - The Amazon S3 artifact bucket for an action\'s artifacts.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { key = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | The artifact name.
s3Location_key :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

-- | The Amazon S3 artifact bucket for an action\'s artifacts.
s3Location_bucket :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

instance Prelude.FromJSON S3Location where
  parseJSON =
    Prelude.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Prelude..:? "key")
            Prelude.<*> (x Prelude..:? "bucket")
      )

instance Prelude.Hashable S3Location

instance Prelude.NFData S3Location
