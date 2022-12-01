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
-- Module      : Amazonka.CloudWatchEvents.Types.Secondary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Secondary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The secondary Region that processes events when failover is triggered or
-- replication is enabled.
--
-- /See:/ 'newSecondary' smart constructor.
data Secondary = Secondary'
  { -- | Defines the secondary Region.
    route :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Secondary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'secondary_route' - Defines the secondary Region.
newSecondary ::
  -- | 'route'
  Prelude.Text ->
  Secondary
newSecondary pRoute_ = Secondary' {route = pRoute_}

-- | Defines the secondary Region.
secondary_route :: Lens.Lens' Secondary Prelude.Text
secondary_route = Lens.lens (\Secondary' {route} -> route) (\s@Secondary' {} a -> s {route = a} :: Secondary)

instance Core.FromJSON Secondary where
  parseJSON =
    Core.withObject
      "Secondary"
      (\x -> Secondary' Prelude.<$> (x Core..: "Route"))

instance Prelude.Hashable Secondary where
  hashWithSalt _salt Secondary' {..} =
    _salt `Prelude.hashWithSalt` route

instance Prelude.NFData Secondary where
  rnf Secondary' {..} = Prelude.rnf route

instance Core.ToJSON Secondary where
  toJSON Secondary' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Route" Core..= route)]
      )
