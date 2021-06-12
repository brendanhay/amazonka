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
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchFilter where

import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies filters when retrieving batch builds.
--
-- /See:/ 'newBuildBatchFilter' smart constructor.
data BuildBatchFilter = BuildBatchFilter'
  { -- | The status of the batch builds to retrieve. Only batch builds that have
    -- this status will be retrieved.
    status :: Core.Maybe StatusType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildBatchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'buildBatchFilter_status' - The status of the batch builds to retrieve. Only batch builds that have
-- this status will be retrieved.
newBuildBatchFilter ::
  BuildBatchFilter
newBuildBatchFilter =
  BuildBatchFilter' {status = Core.Nothing}

-- | The status of the batch builds to retrieve. Only batch builds that have
-- this status will be retrieved.
buildBatchFilter_status :: Lens.Lens' BuildBatchFilter (Core.Maybe StatusType)
buildBatchFilter_status = Lens.lens (\BuildBatchFilter' {status} -> status) (\s@BuildBatchFilter' {} a -> s {status = a} :: BuildBatchFilter)

instance Core.Hashable BuildBatchFilter

instance Core.NFData BuildBatchFilter

instance Core.ToJSON BuildBatchFilter where
  toJSON BuildBatchFilter' {..} =
    Core.object
      (Core.catMaybes [("status" Core..=) Core.<$> status])
