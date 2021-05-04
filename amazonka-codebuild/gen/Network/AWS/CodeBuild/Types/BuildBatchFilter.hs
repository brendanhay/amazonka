{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies filters when retrieving batch builds.
--
-- /See:/ 'newBuildBatchFilter' smart constructor.
data BuildBatchFilter = BuildBatchFilter'
  { -- | The status of the batch builds to retrieve. Only batch builds that have
    -- this status will be retrieved.
    status :: Prelude.Maybe StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  BuildBatchFilter' {status = Prelude.Nothing}

-- | The status of the batch builds to retrieve. Only batch builds that have
-- this status will be retrieved.
buildBatchFilter_status :: Lens.Lens' BuildBatchFilter (Prelude.Maybe StatusType)
buildBatchFilter_status = Lens.lens (\BuildBatchFilter' {status} -> status) (\s@BuildBatchFilter' {} a -> s {status = a} :: BuildBatchFilter)

instance Prelude.Hashable BuildBatchFilter

instance Prelude.NFData BuildBatchFilter

instance Prelude.ToJSON BuildBatchFilter where
  toJSON BuildBatchFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("status" Prelude..=) Prelude.<$> status]
      )
