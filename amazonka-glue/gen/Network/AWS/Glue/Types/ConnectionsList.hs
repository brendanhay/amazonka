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
-- Module      : Network.AWS.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionsList where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the connections used by a job.
--
-- /See:/ 'newConnectionsList' smart constructor.
data ConnectionsList = ConnectionsList'
  { -- | A list of connections used by the job.
    connections :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'connectionsList_connections' - A list of connections used by the job.
newConnectionsList ::
  ConnectionsList
newConnectionsList =
  ConnectionsList' {connections = Core.Nothing}

-- | A list of connections used by the job.
connectionsList_connections :: Lens.Lens' ConnectionsList (Core.Maybe [Core.Text])
connectionsList_connections = Lens.lens (\ConnectionsList' {connections} -> connections) (\s@ConnectionsList' {} a -> s {connections = a} :: ConnectionsList) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ConnectionsList where
  parseJSON =
    Core.withObject
      "ConnectionsList"
      ( \x ->
          ConnectionsList'
            Core.<$> (x Core..:? "Connections" Core..!= Core.mempty)
      )

instance Core.Hashable ConnectionsList

instance Core.NFData ConnectionsList

instance Core.ToJSON ConnectionsList where
  toJSON ConnectionsList' {..} =
    Core.object
      ( Core.catMaybes
          [("Connections" Core..=) Core.<$> connections]
      )
