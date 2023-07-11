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
-- Module      : Amazonka.RobOMaker.Types.Source
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.Architecture

-- | Information about a source.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | The taget processor architecture for the application.
    architecture :: Prelude.Maybe Architecture,
    -- | A hash of the object specified by @s3Bucket@ and @s3Key@.
    etag :: Prelude.Maybe Prelude.Text,
    -- | The s3 bucket name.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The s3 object key.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'source_architecture' - The taget processor architecture for the application.
--
-- 'etag', 'source_etag' - A hash of the object specified by @s3Bucket@ and @s3Key@.
--
-- 's3Bucket', 'source_s3Bucket' - The s3 bucket name.
--
-- 's3Key', 'source_s3Key' - The s3 object key.
newSource ::
  Source
newSource =
  Source'
    { architecture = Prelude.Nothing,
      etag = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The taget processor architecture for the application.
source_architecture :: Lens.Lens' Source (Prelude.Maybe Architecture)
source_architecture = Lens.lens (\Source' {architecture} -> architecture) (\s@Source' {} a -> s {architecture = a} :: Source)

-- | A hash of the object specified by @s3Bucket@ and @s3Key@.
source_etag :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_etag = Lens.lens (\Source' {etag} -> etag) (\s@Source' {} a -> s {etag = a} :: Source)

-- | The s3 bucket name.
source_s3Bucket :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_s3Bucket = Lens.lens (\Source' {s3Bucket} -> s3Bucket) (\s@Source' {} a -> s {s3Bucket = a} :: Source)

-- | The s3 object key.
source_s3Key :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_s3Key = Lens.lens (\Source' {s3Key} -> s3Key) (\s@Source' {} a -> s {s3Key = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Data..:? "architecture")
            Prelude.<*> (x Data..:? "etag")
            Prelude.<*> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3Key")
      )

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` etag
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf etag
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
