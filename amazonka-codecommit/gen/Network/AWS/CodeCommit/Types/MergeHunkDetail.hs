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
-- Module      : Network.AWS.CodeCommit.Types.MergeHunkDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeHunkDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the details of a merge hunk that contains a conflict
-- in a merge or pull request operation.
--
-- /See:/ 'newMergeHunkDetail' smart constructor.
data MergeHunkDetail = MergeHunkDetail'
  { -- | The base-64 encoded content of the hunk merged region that might contain
    -- a conflict.
    hunkContent :: Core.Maybe Core.Text,
    -- | The start position of the hunk in the merge result.
    startLine :: Core.Maybe Core.Int,
    -- | The end position of the hunk in the merge result.
    endLine :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MergeHunkDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hunkContent', 'mergeHunkDetail_hunkContent' - The base-64 encoded content of the hunk merged region that might contain
-- a conflict.
--
-- 'startLine', 'mergeHunkDetail_startLine' - The start position of the hunk in the merge result.
--
-- 'endLine', 'mergeHunkDetail_endLine' - The end position of the hunk in the merge result.
newMergeHunkDetail ::
  MergeHunkDetail
newMergeHunkDetail =
  MergeHunkDetail'
    { hunkContent = Core.Nothing,
      startLine = Core.Nothing,
      endLine = Core.Nothing
    }

-- | The base-64 encoded content of the hunk merged region that might contain
-- a conflict.
mergeHunkDetail_hunkContent :: Lens.Lens' MergeHunkDetail (Core.Maybe Core.Text)
mergeHunkDetail_hunkContent = Lens.lens (\MergeHunkDetail' {hunkContent} -> hunkContent) (\s@MergeHunkDetail' {} a -> s {hunkContent = a} :: MergeHunkDetail)

-- | The start position of the hunk in the merge result.
mergeHunkDetail_startLine :: Lens.Lens' MergeHunkDetail (Core.Maybe Core.Int)
mergeHunkDetail_startLine = Lens.lens (\MergeHunkDetail' {startLine} -> startLine) (\s@MergeHunkDetail' {} a -> s {startLine = a} :: MergeHunkDetail)

-- | The end position of the hunk in the merge result.
mergeHunkDetail_endLine :: Lens.Lens' MergeHunkDetail (Core.Maybe Core.Int)
mergeHunkDetail_endLine = Lens.lens (\MergeHunkDetail' {endLine} -> endLine) (\s@MergeHunkDetail' {} a -> s {endLine = a} :: MergeHunkDetail)

instance Core.FromJSON MergeHunkDetail where
  parseJSON =
    Core.withObject
      "MergeHunkDetail"
      ( \x ->
          MergeHunkDetail'
            Core.<$> (x Core..:? "hunkContent")
            Core.<*> (x Core..:? "startLine")
            Core.<*> (x Core..:? "endLine")
      )

instance Core.Hashable MergeHunkDetail

instance Core.NFData MergeHunkDetail
