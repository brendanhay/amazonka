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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkSegmentEdgeIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkSegmentEdgeIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns details about a core network edge.
--
-- /See:/ 'newCoreNetworkSegmentEdgeIdentifier' smart constructor.
data CoreNetworkSegmentEdgeIdentifier = CoreNetworkSegmentEdgeIdentifier'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The Region where the segment edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The name of the segment edge.
    segmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkSegmentEdgeIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'coreNetworkSegmentEdgeIdentifier_coreNetworkId' - The ID of a core network.
--
-- 'edgeLocation', 'coreNetworkSegmentEdgeIdentifier_edgeLocation' - The Region where the segment edge is located.
--
-- 'segmentName', 'coreNetworkSegmentEdgeIdentifier_segmentName' - The name of the segment edge.
newCoreNetworkSegmentEdgeIdentifier ::
  CoreNetworkSegmentEdgeIdentifier
newCoreNetworkSegmentEdgeIdentifier =
  CoreNetworkSegmentEdgeIdentifier'
    { coreNetworkId =
        Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      segmentName = Prelude.Nothing
    }

-- | The ID of a core network.
coreNetworkSegmentEdgeIdentifier_coreNetworkId :: Lens.Lens' CoreNetworkSegmentEdgeIdentifier (Prelude.Maybe Prelude.Text)
coreNetworkSegmentEdgeIdentifier_coreNetworkId = Lens.lens (\CoreNetworkSegmentEdgeIdentifier' {coreNetworkId} -> coreNetworkId) (\s@CoreNetworkSegmentEdgeIdentifier' {} a -> s {coreNetworkId = a} :: CoreNetworkSegmentEdgeIdentifier)

-- | The Region where the segment edge is located.
coreNetworkSegmentEdgeIdentifier_edgeLocation :: Lens.Lens' CoreNetworkSegmentEdgeIdentifier (Prelude.Maybe Prelude.Text)
coreNetworkSegmentEdgeIdentifier_edgeLocation = Lens.lens (\CoreNetworkSegmentEdgeIdentifier' {edgeLocation} -> edgeLocation) (\s@CoreNetworkSegmentEdgeIdentifier' {} a -> s {edgeLocation = a} :: CoreNetworkSegmentEdgeIdentifier)

-- | The name of the segment edge.
coreNetworkSegmentEdgeIdentifier_segmentName :: Lens.Lens' CoreNetworkSegmentEdgeIdentifier (Prelude.Maybe Prelude.Text)
coreNetworkSegmentEdgeIdentifier_segmentName = Lens.lens (\CoreNetworkSegmentEdgeIdentifier' {segmentName} -> segmentName) (\s@CoreNetworkSegmentEdgeIdentifier' {} a -> s {segmentName = a} :: CoreNetworkSegmentEdgeIdentifier)

instance
  Core.FromJSON
    CoreNetworkSegmentEdgeIdentifier
  where
  parseJSON =
    Core.withObject
      "CoreNetworkSegmentEdgeIdentifier"
      ( \x ->
          CoreNetworkSegmentEdgeIdentifier'
            Prelude.<$> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "EdgeLocation")
            Prelude.<*> (x Core..:? "SegmentName")
      )

instance
  Prelude.Hashable
    CoreNetworkSegmentEdgeIdentifier
  where
  hashWithSalt
    _salt
    CoreNetworkSegmentEdgeIdentifier' {..} =
      _salt `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` edgeLocation
        `Prelude.hashWithSalt` segmentName

instance
  Prelude.NFData
    CoreNetworkSegmentEdgeIdentifier
  where
  rnf CoreNetworkSegmentEdgeIdentifier' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf segmentName

instance Core.ToJSON CoreNetworkSegmentEdgeIdentifier where
  toJSON CoreNetworkSegmentEdgeIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CoreNetworkId" Core..=) Prelude.<$> coreNetworkId,
            ("EdgeLocation" Core..=) Prelude.<$> edgeLocation,
            ("SegmentName" Core..=) Prelude.<$> segmentName
          ]
      )
