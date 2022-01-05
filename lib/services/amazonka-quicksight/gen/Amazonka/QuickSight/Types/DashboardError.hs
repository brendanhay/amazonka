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
-- Module      : Amazonka.QuickSight.Types.DashboardError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardErrorType

-- | Dashboard error.
--
-- /See:/ 'newDashboardError' smart constructor.
data DashboardError = DashboardError'
  { -- | Type.
    type' :: Prelude.Maybe DashboardErrorType,
    -- | Message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'dashboardError_type' - Type.
--
-- 'message', 'dashboardError_message' - Message.
newDashboardError ::
  DashboardError
newDashboardError =
  DashboardError'
    { type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Type.
dashboardError_type :: Lens.Lens' DashboardError (Prelude.Maybe DashboardErrorType)
dashboardError_type = Lens.lens (\DashboardError' {type'} -> type') (\s@DashboardError' {} a -> s {type' = a} :: DashboardError)

-- | Message.
dashboardError_message :: Lens.Lens' DashboardError (Prelude.Maybe Prelude.Text)
dashboardError_message = Lens.lens (\DashboardError' {message} -> message) (\s@DashboardError' {} a -> s {message = a} :: DashboardError)

instance Core.FromJSON DashboardError where
  parseJSON =
    Core.withObject
      "DashboardError"
      ( \x ->
          DashboardError'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable DashboardError where
  hashWithSalt _salt DashboardError' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` message

instance Prelude.NFData DashboardError where
  rnf DashboardError' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf message
