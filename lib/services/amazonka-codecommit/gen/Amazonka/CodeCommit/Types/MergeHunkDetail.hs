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
-- Module      : Amazonka.CodeCommit.Types.MergeHunkDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.MergeHunkDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the details of a merge hunk that contains a conflict
-- in a merge or pull request operation.
--
-- /See:/ 'newMergeHunkDetail' smart constructor.
data MergeHunkDetail = MergeHunkDetail'
  { -- | The end position of the hunk in the merge result.
    endLine :: Prelude.Maybe Prelude.Int,
    -- | The base-64 encoded content of the hunk merged region that might contain
    -- a conflict.
    hunkContent :: Prelude.Maybe Prelude.Text,
    -- | The start position of the hunk in the merge result.
    startLine :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeHunkDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endLine', 'mergeHunkDetail_endLine' - The end position of the hunk in the merge result.
--
-- 'hunkContent', 'mergeHunkDetail_hunkContent' - The base-64 encoded content of the hunk merged region that might contain
-- a conflict.
--
-- 'startLine', 'mergeHunkDetail_startLine' - The start position of the hunk in the merge result.
newMergeHunkDetail ::
  MergeHunkDetail
newMergeHunkDetail =
  MergeHunkDetail'
    { endLine = Prelude.Nothing,
      hunkContent = Prelude.Nothing,
      startLine = Prelude.Nothing
    }

-- | The end position of the hunk in the merge result.
mergeHunkDetail_endLine :: Lens.Lens' MergeHunkDetail (Prelude.Maybe Prelude.Int)
mergeHunkDetail_endLine = Lens.lens (\MergeHunkDetail' {endLine} -> endLine) (\s@MergeHunkDetail' {} a -> s {endLine = a} :: MergeHunkDetail)

-- | The base-64 encoded content of the hunk merged region that might contain
-- a conflict.
mergeHunkDetail_hunkContent :: Lens.Lens' MergeHunkDetail (Prelude.Maybe Prelude.Text)
mergeHunkDetail_hunkContent = Lens.lens (\MergeHunkDetail' {hunkContent} -> hunkContent) (\s@MergeHunkDetail' {} a -> s {hunkContent = a} :: MergeHunkDetail)

-- | The start position of the hunk in the merge result.
mergeHunkDetail_startLine :: Lens.Lens' MergeHunkDetail (Prelude.Maybe Prelude.Int)
mergeHunkDetail_startLine = Lens.lens (\MergeHunkDetail' {startLine} -> startLine) (\s@MergeHunkDetail' {} a -> s {startLine = a} :: MergeHunkDetail)

instance Data.FromJSON MergeHunkDetail where
  parseJSON =
    Data.withObject
      "MergeHunkDetail"
      ( \x ->
          MergeHunkDetail'
            Prelude.<$> (x Data..:? "endLine")
            Prelude.<*> (x Data..:? "hunkContent")
            Prelude.<*> (x Data..:? "startLine")
      )

instance Prelude.Hashable MergeHunkDetail where
  hashWithSalt _salt MergeHunkDetail' {..} =
    _salt `Prelude.hashWithSalt` endLine
      `Prelude.hashWithSalt` hunkContent
      `Prelude.hashWithSalt` startLine

instance Prelude.NFData MergeHunkDetail where
  rnf MergeHunkDetail' {..} =
    Prelude.rnf endLine
      `Prelude.seq` Prelude.rnf hunkContent
      `Prelude.seq` Prelude.rnf startLine
