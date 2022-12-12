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
-- Module      : Amazonka.MacieV2.Types.S3ClassificationScope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3ClassificationScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3ClassificationScopeExclusion
import qualified Amazonka.Prelude as Prelude

-- | Specifies the S3 buckets that are excluded from automated sensitive data
-- discovery for an Amazon Macie account.
--
-- /See:/ 'newS3ClassificationScope' smart constructor.
data S3ClassificationScope = S3ClassificationScope'
  { -- | The S3 buckets that are excluded.
    excludes :: S3ClassificationScopeExclusion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ClassificationScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 's3ClassificationScope_excludes' - The S3 buckets that are excluded.
newS3ClassificationScope ::
  -- | 'excludes'
  S3ClassificationScopeExclusion ->
  S3ClassificationScope
newS3ClassificationScope pExcludes_ =
  S3ClassificationScope' {excludes = pExcludes_}

-- | The S3 buckets that are excluded.
s3ClassificationScope_excludes :: Lens.Lens' S3ClassificationScope S3ClassificationScopeExclusion
s3ClassificationScope_excludes = Lens.lens (\S3ClassificationScope' {excludes} -> excludes) (\s@S3ClassificationScope' {} a -> s {excludes = a} :: S3ClassificationScope)

instance Data.FromJSON S3ClassificationScope where
  parseJSON =
    Data.withObject
      "S3ClassificationScope"
      ( \x ->
          S3ClassificationScope'
            Prelude.<$> (x Data..: "excludes")
      )

instance Prelude.Hashable S3ClassificationScope where
  hashWithSalt _salt S3ClassificationScope' {..} =
    _salt `Prelude.hashWithSalt` excludes

instance Prelude.NFData S3ClassificationScope where
  rnf S3ClassificationScope' {..} = Prelude.rnf excludes
