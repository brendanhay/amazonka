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
-- Module      : Amazonka.MacieV2.Types.S3ClassificationScopeUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3ClassificationScopeUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3ClassificationScopeExclusionUpdate
import qualified Amazonka.Prelude as Prelude

-- | Specifies changes to the list of S3 buckets that are excluded from
-- automated sensitive data discovery for an Amazon Macie account.
--
-- /See:/ 'newS3ClassificationScopeUpdate' smart constructor.
data S3ClassificationScopeUpdate = S3ClassificationScopeUpdate'
  { -- | The names of the S3 buckets to add or remove from the list.
    excludes :: S3ClassificationScopeExclusionUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ClassificationScopeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 's3ClassificationScopeUpdate_excludes' - The names of the S3 buckets to add or remove from the list.
newS3ClassificationScopeUpdate ::
  -- | 'excludes'
  S3ClassificationScopeExclusionUpdate ->
  S3ClassificationScopeUpdate
newS3ClassificationScopeUpdate pExcludes_ =
  S3ClassificationScopeUpdate' {excludes = pExcludes_}

-- | The names of the S3 buckets to add or remove from the list.
s3ClassificationScopeUpdate_excludes :: Lens.Lens' S3ClassificationScopeUpdate S3ClassificationScopeExclusionUpdate
s3ClassificationScopeUpdate_excludes = Lens.lens (\S3ClassificationScopeUpdate' {excludes} -> excludes) (\s@S3ClassificationScopeUpdate' {} a -> s {excludes = a} :: S3ClassificationScopeUpdate)

instance Prelude.Hashable S3ClassificationScopeUpdate where
  hashWithSalt _salt S3ClassificationScopeUpdate' {..} =
    _salt `Prelude.hashWithSalt` excludes

instance Prelude.NFData S3ClassificationScopeUpdate where
  rnf S3ClassificationScopeUpdate' {..} =
    Prelude.rnf excludes

instance Data.ToJSON S3ClassificationScopeUpdate where
  toJSON S3ClassificationScopeUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("excludes" Data..= excludes)]
      )
