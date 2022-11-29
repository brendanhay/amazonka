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
-- Module      : Amazonka.RobOMaker.Types.OutputLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.OutputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The output location.
--
-- /See:/ 'newOutputLocation' smart constructor.
data OutputLocation = OutputLocation'
  { -- | The S3 bucket for output.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The S3 folder in the @s3Bucket@ where output files will be placed.
    s3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'outputLocation_s3Bucket' - The S3 bucket for output.
--
-- 's3Prefix', 'outputLocation_s3Prefix' - The S3 folder in the @s3Bucket@ where output files will be placed.
newOutputLocation ::
  OutputLocation
newOutputLocation =
  OutputLocation'
    { s3Bucket = Prelude.Nothing,
      s3Prefix = Prelude.Nothing
    }

-- | The S3 bucket for output.
outputLocation_s3Bucket :: Lens.Lens' OutputLocation (Prelude.Maybe Prelude.Text)
outputLocation_s3Bucket = Lens.lens (\OutputLocation' {s3Bucket} -> s3Bucket) (\s@OutputLocation' {} a -> s {s3Bucket = a} :: OutputLocation)

-- | The S3 folder in the @s3Bucket@ where output files will be placed.
outputLocation_s3Prefix :: Lens.Lens' OutputLocation (Prelude.Maybe Prelude.Text)
outputLocation_s3Prefix = Lens.lens (\OutputLocation' {s3Prefix} -> s3Prefix) (\s@OutputLocation' {} a -> s {s3Prefix = a} :: OutputLocation)

instance Core.FromJSON OutputLocation where
  parseJSON =
    Core.withObject
      "OutputLocation"
      ( \x ->
          OutputLocation'
            Prelude.<$> (x Core..:? "s3Bucket")
            Prelude.<*> (x Core..:? "s3Prefix")
      )

instance Prelude.Hashable OutputLocation where
  hashWithSalt _salt OutputLocation' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Prefix

instance Prelude.NFData OutputLocation where
  rnf OutputLocation' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Prefix

instance Core.ToJSON OutputLocation where
  toJSON OutputLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3Bucket" Core..=) Prelude.<$> s3Bucket,
            ("s3Prefix" Core..=) Prelude.<$> s3Prefix
          ]
      )
