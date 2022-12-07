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
-- Module      : Amazonka.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.S3Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the location of updated firmware in S3.
--
-- /See:/ 'newS3Destination' smart constructor.
data S3Destination = S3Destination'
  { -- | The S3 bucket that contains the updated firmware.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The S3 prefix.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Destination_bucket' - The S3 bucket that contains the updated firmware.
--
-- 'prefix', 's3Destination_prefix' - The S3 prefix.
newS3Destination ::
  S3Destination
newS3Destination =
  S3Destination'
    { bucket = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The S3 bucket that contains the updated firmware.
s3Destination_bucket :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_bucket = Lens.lens (\S3Destination' {bucket} -> bucket) (\s@S3Destination' {} a -> s {bucket = a} :: S3Destination)

-- | The S3 prefix.
s3Destination_prefix :: Lens.Lens' S3Destination (Prelude.Maybe Prelude.Text)
s3Destination_prefix = Lens.lens (\S3Destination' {prefix} -> prefix) (\s@S3Destination' {} a -> s {prefix = a} :: S3Destination)

instance Data.FromJSON S3Destination where
  parseJSON =
    Data.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "prefix")
      )

instance Prelude.Hashable S3Destination where
  hashWithSalt _salt S3Destination' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Destination where
  rnf S3Destination' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("prefix" Data..=) Prelude.<$> prefix
          ]
      )
