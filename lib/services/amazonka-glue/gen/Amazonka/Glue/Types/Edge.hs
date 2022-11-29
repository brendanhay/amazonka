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
-- Module      : Amazonka.Glue.Types.Edge
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Edge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An edge represents a directed connection between two Glue components
-- that are part of the workflow the edge belongs to.
--
-- /See:/ 'newEdge' smart constructor.
data Edge = Edge'
  { -- | The unique of the node within the workflow where the edge starts.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The unique of the node within the workflow where the edge ends.
    destinationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Edge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'edge_sourceId' - The unique of the node within the workflow where the edge starts.
--
-- 'destinationId', 'edge_destinationId' - The unique of the node within the workflow where the edge ends.
newEdge ::
  Edge
newEdge =
  Edge'
    { sourceId = Prelude.Nothing,
      destinationId = Prelude.Nothing
    }

-- | The unique of the node within the workflow where the edge starts.
edge_sourceId :: Lens.Lens' Edge (Prelude.Maybe Prelude.Text)
edge_sourceId = Lens.lens (\Edge' {sourceId} -> sourceId) (\s@Edge' {} a -> s {sourceId = a} :: Edge)

-- | The unique of the node within the workflow where the edge ends.
edge_destinationId :: Lens.Lens' Edge (Prelude.Maybe Prelude.Text)
edge_destinationId = Lens.lens (\Edge' {destinationId} -> destinationId) (\s@Edge' {} a -> s {destinationId = a} :: Edge)

instance Core.FromJSON Edge where
  parseJSON =
    Core.withObject
      "Edge"
      ( \x ->
          Edge'
            Prelude.<$> (x Core..:? "SourceId")
            Prelude.<*> (x Core..:? "DestinationId")
      )

instance Prelude.Hashable Edge where
  hashWithSalt _salt Edge' {..} =
    _salt `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` destinationId

instance Prelude.NFData Edge where
  rnf Edge' {..} =
    Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf destinationId
