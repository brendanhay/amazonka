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
-- Module      : Amazonka.SWF.Types.CloseStatusFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.CloseStatusFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.CloseStatus

-- | Used to filter the closed workflow executions in visibility APIs by
-- their close status.
--
-- /See:/ 'newCloseStatusFilter' smart constructor.
data CloseStatusFilter = CloseStatusFilter'
  { -- | The close status that must match the close status of an execution for it
    -- to meet the criteria of this filter.
    status :: CloseStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloseStatusFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'closeStatusFilter_status' - The close status that must match the close status of an execution for it
-- to meet the criteria of this filter.
newCloseStatusFilter ::
  -- | 'status'
  CloseStatus ->
  CloseStatusFilter
newCloseStatusFilter pStatus_ =
  CloseStatusFilter' {status = pStatus_}

-- | The close status that must match the close status of an execution for it
-- to meet the criteria of this filter.
closeStatusFilter_status :: Lens.Lens' CloseStatusFilter CloseStatus
closeStatusFilter_status = Lens.lens (\CloseStatusFilter' {status} -> status) (\s@CloseStatusFilter' {} a -> s {status = a} :: CloseStatusFilter)

instance Prelude.Hashable CloseStatusFilter where
  hashWithSalt _salt CloseStatusFilter' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData CloseStatusFilter where
  rnf CloseStatusFilter' {..} = Prelude.rnf status

instance Data.ToJSON CloseStatusFilter where
  toJSON CloseStatusFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )
