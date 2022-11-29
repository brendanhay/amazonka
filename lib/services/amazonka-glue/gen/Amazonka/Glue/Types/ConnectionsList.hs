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
-- Module      : Amazonka.Glue.Types.ConnectionsList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ConnectionsList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the connections used by a job.
--
-- /See:/ 'newConnectionsList' smart constructor.
data ConnectionsList = ConnectionsList'
  { -- | A list of connections used by the job.
    connections :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  ConnectionsList' {connections = Prelude.Nothing}

-- | A list of connections used by the job.
connectionsList_connections :: Lens.Lens' ConnectionsList (Prelude.Maybe [Prelude.Text])
connectionsList_connections = Lens.lens (\ConnectionsList' {connections} -> connections) (\s@ConnectionsList' {} a -> s {connections = a} :: ConnectionsList) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConnectionsList where
  parseJSON =
    Core.withObject
      "ConnectionsList"
      ( \x ->
          ConnectionsList'
            Prelude.<$> (x Core..:? "Connections" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ConnectionsList where
  hashWithSalt _salt ConnectionsList' {..} =
    _salt `Prelude.hashWithSalt` connections

instance Prelude.NFData ConnectionsList where
  rnf ConnectionsList' {..} = Prelude.rnf connections

instance Core.ToJSON ConnectionsList where
  toJSON ConnectionsList' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Connections" Core..=) Prelude.<$> connections]
      )
