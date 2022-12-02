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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardErrorType

-- | Dashboard error.
--
-- /See:/ 'newDashboardError' smart constructor.
data DashboardError = DashboardError'
  { -- | Message.
    message :: Prelude.Maybe Prelude.Text,
    -- | Type.
    type' :: Prelude.Maybe DashboardErrorType
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
-- 'message', 'dashboardError_message' - Message.
--
-- 'type'', 'dashboardError_type' - Type.
newDashboardError ::
  DashboardError
newDashboardError =
  DashboardError'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Message.
dashboardError_message :: Lens.Lens' DashboardError (Prelude.Maybe Prelude.Text)
dashboardError_message = Lens.lens (\DashboardError' {message} -> message) (\s@DashboardError' {} a -> s {message = a} :: DashboardError)

-- | Type.
dashboardError_type :: Lens.Lens' DashboardError (Prelude.Maybe DashboardErrorType)
dashboardError_type = Lens.lens (\DashboardError' {type'} -> type') (\s@DashboardError' {} a -> s {type' = a} :: DashboardError)

instance Data.FromJSON DashboardError where
  parseJSON =
    Data.withObject
      "DashboardError"
      ( \x ->
          DashboardError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DashboardError where
  hashWithSalt _salt DashboardError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DashboardError where
  rnf DashboardError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
