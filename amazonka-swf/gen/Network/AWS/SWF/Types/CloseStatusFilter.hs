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
-- Module      : Network.AWS.SWF.Types.CloseStatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatusFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.CloseStatus

-- | Used to filter the closed workflow executions in visibility APIs by
-- their close status.
--
-- /See:/ 'newCloseStatusFilter' smart constructor.
data CloseStatusFilter = CloseStatusFilter'
  { -- | The close status that must match the close status of an execution for it
    -- to meet the criteria of this filter.
    status :: CloseStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable CloseStatusFilter

instance Prelude.NFData CloseStatusFilter

instance Prelude.ToJSON CloseStatusFilter where
  toJSON CloseStatusFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Prelude..= status)]
      )
