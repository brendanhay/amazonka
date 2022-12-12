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
-- Module      : Amazonka.RobOMaker.Types.S3KeyOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.S3KeyOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about S3 keys.
--
-- /See:/ 'newS3KeyOutput' smart constructor.
data S3KeyOutput = S3KeyOutput'
  { -- | The etag for the object.
    etag :: Prelude.Maybe Prelude.Text,
    -- | The S3 key.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3KeyOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'etag', 's3KeyOutput_etag' - The etag for the object.
--
-- 's3Key', 's3KeyOutput_s3Key' - The S3 key.
newS3KeyOutput ::
  S3KeyOutput
newS3KeyOutput =
  S3KeyOutput'
    { etag = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The etag for the object.
s3KeyOutput_etag :: Lens.Lens' S3KeyOutput (Prelude.Maybe Prelude.Text)
s3KeyOutput_etag = Lens.lens (\S3KeyOutput' {etag} -> etag) (\s@S3KeyOutput' {} a -> s {etag = a} :: S3KeyOutput)

-- | The S3 key.
s3KeyOutput_s3Key :: Lens.Lens' S3KeyOutput (Prelude.Maybe Prelude.Text)
s3KeyOutput_s3Key = Lens.lens (\S3KeyOutput' {s3Key} -> s3Key) (\s@S3KeyOutput' {} a -> s {s3Key = a} :: S3KeyOutput)

instance Data.FromJSON S3KeyOutput where
  parseJSON =
    Data.withObject
      "S3KeyOutput"
      ( \x ->
          S3KeyOutput'
            Prelude.<$> (x Data..:? "etag") Prelude.<*> (x Data..:? "s3Key")
      )

instance Prelude.Hashable S3KeyOutput where
  hashWithSalt _salt S3KeyOutput' {..} =
    _salt `Prelude.hashWithSalt` etag
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData S3KeyOutput where
  rnf S3KeyOutput' {..} =
    Prelude.rnf etag `Prelude.seq` Prelude.rnf s3Key
