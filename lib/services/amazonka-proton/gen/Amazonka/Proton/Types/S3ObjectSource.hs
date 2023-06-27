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
-- Module      : Amazonka.Proton.Types.S3ObjectSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.S3ObjectSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Template bundle S3 bucket data.
--
-- /See:/ 'newS3ObjectSource' smart constructor.
data S3ObjectSource = S3ObjectSource'
  { -- | The name of the S3 bucket that contains a template bundle.
    bucket :: Prelude.Text,
    -- | The path to the S3 bucket that contains a template bundle.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ObjectSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3ObjectSource_bucket' - The name of the S3 bucket that contains a template bundle.
--
-- 'key', 's3ObjectSource_key' - The path to the S3 bucket that contains a template bundle.
newS3ObjectSource ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3ObjectSource
newS3ObjectSource pBucket_ pKey_ =
  S3ObjectSource' {bucket = pBucket_, key = pKey_}

-- | The name of the S3 bucket that contains a template bundle.
s3ObjectSource_bucket :: Lens.Lens' S3ObjectSource Prelude.Text
s3ObjectSource_bucket = Lens.lens (\S3ObjectSource' {bucket} -> bucket) (\s@S3ObjectSource' {} a -> s {bucket = a} :: S3ObjectSource)

-- | The path to the S3 bucket that contains a template bundle.
s3ObjectSource_key :: Lens.Lens' S3ObjectSource Prelude.Text
s3ObjectSource_key = Lens.lens (\S3ObjectSource' {key} -> key) (\s@S3ObjectSource' {} a -> s {key = a} :: S3ObjectSource)

instance Prelude.Hashable S3ObjectSource where
  hashWithSalt _salt S3ObjectSource' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3ObjectSource where
  rnf S3ObjectSource' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance Data.ToJSON S3ObjectSource where
  toJSON S3ObjectSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("key" Data..= key)
          ]
      )
