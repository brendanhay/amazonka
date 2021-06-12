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
-- Module      : Network.AWS.Glue.Types.Edge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Edge where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An edge represents a directed connection between two AWS Glue components
-- that are part of the workflow the edge belongs to.
--
-- /See:/ 'newEdge' smart constructor.
data Edge = Edge'
  { -- | The unique of the node within the workflow where the edge ends.
    destinationId :: Core.Maybe Core.Text,
    -- | The unique of the node within the workflow where the edge starts.
    sourceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Edge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationId', 'edge_destinationId' - The unique of the node within the workflow where the edge ends.
--
-- 'sourceId', 'edge_sourceId' - The unique of the node within the workflow where the edge starts.
newEdge ::
  Edge
newEdge =
  Edge'
    { destinationId = Core.Nothing,
      sourceId = Core.Nothing
    }

-- | The unique of the node within the workflow where the edge ends.
edge_destinationId :: Lens.Lens' Edge (Core.Maybe Core.Text)
edge_destinationId = Lens.lens (\Edge' {destinationId} -> destinationId) (\s@Edge' {} a -> s {destinationId = a} :: Edge)

-- | The unique of the node within the workflow where the edge starts.
edge_sourceId :: Lens.Lens' Edge (Core.Maybe Core.Text)
edge_sourceId = Lens.lens (\Edge' {sourceId} -> sourceId) (\s@Edge' {} a -> s {sourceId = a} :: Edge)

instance Core.FromJSON Edge where
  parseJSON =
    Core.withObject
      "Edge"
      ( \x ->
          Edge'
            Core.<$> (x Core..:? "DestinationId")
            Core.<*> (x Core..:? "SourceId")
      )

instance Core.Hashable Edge

instance Core.NFData Edge
