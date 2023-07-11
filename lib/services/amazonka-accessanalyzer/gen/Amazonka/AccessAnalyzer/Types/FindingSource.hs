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
-- Module      : Amazonka.AccessAnalyzer.Types.FindingSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.FindingSource where

import Amazonka.AccessAnalyzer.Types.FindingSourceDetail
import Amazonka.AccessAnalyzer.Types.FindingSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the finding. This indicates how the access that generated
-- the finding is granted. It is populated for Amazon S3 bucket findings.
--
-- /See:/ 'newFindingSource' smart constructor.
data FindingSource = FindingSource'
  { -- | Includes details about how the access that generated the finding is
    -- granted. This is populated for Amazon S3 bucket findings.
    detail :: Prelude.Maybe FindingSourceDetail,
    -- | Indicates the type of access that generated the finding.
    type' :: FindingSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detail', 'findingSource_detail' - Includes details about how the access that generated the finding is
-- granted. This is populated for Amazon S3 bucket findings.
--
-- 'type'', 'findingSource_type' - Indicates the type of access that generated the finding.
newFindingSource ::
  -- | 'type''
  FindingSourceType ->
  FindingSource
newFindingSource pType_ =
  FindingSource'
    { detail = Prelude.Nothing,
      type' = pType_
    }

-- | Includes details about how the access that generated the finding is
-- granted. This is populated for Amazon S3 bucket findings.
findingSource_detail :: Lens.Lens' FindingSource (Prelude.Maybe FindingSourceDetail)
findingSource_detail = Lens.lens (\FindingSource' {detail} -> detail) (\s@FindingSource' {} a -> s {detail = a} :: FindingSource)

-- | Indicates the type of access that generated the finding.
findingSource_type :: Lens.Lens' FindingSource FindingSourceType
findingSource_type = Lens.lens (\FindingSource' {type'} -> type') (\s@FindingSource' {} a -> s {type' = a} :: FindingSource)

instance Data.FromJSON FindingSource where
  parseJSON =
    Data.withObject
      "FindingSource"
      ( \x ->
          FindingSource'
            Prelude.<$> (x Data..:? "detail")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FindingSource where
  hashWithSalt _salt FindingSource' {..} =
    _salt
      `Prelude.hashWithSalt` detail
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FindingSource where
  rnf FindingSource' {..} =
    Prelude.rnf detail `Prelude.seq` Prelude.rnf type'
