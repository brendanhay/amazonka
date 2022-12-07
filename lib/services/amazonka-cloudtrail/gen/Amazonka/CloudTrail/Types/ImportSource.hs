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
-- Module      : Amazonka.CloudTrail.Types.ImportSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ImportSource where

import Amazonka.CloudTrail.Types.S3ImportSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The import source.
--
-- /See:/ 'newImportSource' smart constructor.
data ImportSource = ImportSource'
  { -- | The source S3 bucket.
    s3 :: S3ImportSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'importSource_s3' - The source S3 bucket.
newImportSource ::
  -- | 's3'
  S3ImportSource ->
  ImportSource
newImportSource pS3_ = ImportSource' {s3 = pS3_}

-- | The source S3 bucket.
importSource_s3 :: Lens.Lens' ImportSource S3ImportSource
importSource_s3 = Lens.lens (\ImportSource' {s3} -> s3) (\s@ImportSource' {} a -> s {s3 = a} :: ImportSource)

instance Data.FromJSON ImportSource where
  parseJSON =
    Data.withObject
      "ImportSource"
      (\x -> ImportSource' Prelude.<$> (x Data..: "S3"))

instance Prelude.Hashable ImportSource where
  hashWithSalt _salt ImportSource' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData ImportSource where
  rnf ImportSource' {..} = Prelude.rnf s3

instance Data.ToJSON ImportSource where
  toJSON ImportSource' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("S3" Data..= s3)])
