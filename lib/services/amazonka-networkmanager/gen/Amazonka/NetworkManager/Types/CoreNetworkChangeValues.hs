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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkChangeValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkChangeValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network change.
--
-- /See:/ 'newCoreNetworkChangeValues' smart constructor.
data CoreNetworkChangeValues = CoreNetworkChangeValues'
  { -- | The shared segments for a core network change value.
    sharedSegments :: Prelude.Maybe [Prelude.Text],
    -- | The IP addresses used for a core network.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The ASN of a core network.
    asn :: Prelude.Maybe Prelude.Integer,
    -- | The names of the segments in a core network.
    segmentName :: Prelude.Maybe Prelude.Text,
    -- | The Regions where edges are located in a core network.
    edgeLocations :: Prelude.Maybe [Prelude.Text],
    -- | The inside IP addresses used for core network change values.
    insideCidrBlocks :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the destination.
    destinationIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkChangeValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedSegments', 'coreNetworkChangeValues_sharedSegments' - The shared segments for a core network change value.
--
-- 'cidr', 'coreNetworkChangeValues_cidr' - The IP addresses used for a core network.
--
-- 'asn', 'coreNetworkChangeValues_asn' - The ASN of a core network.
--
-- 'segmentName', 'coreNetworkChangeValues_segmentName' - The names of the segments in a core network.
--
-- 'edgeLocations', 'coreNetworkChangeValues_edgeLocations' - The Regions where edges are located in a core network.
--
-- 'insideCidrBlocks', 'coreNetworkChangeValues_insideCidrBlocks' - The inside IP addresses used for core network change values.
--
-- 'destinationIdentifier', 'coreNetworkChangeValues_destinationIdentifier' - The ID of the destination.
newCoreNetworkChangeValues ::
  CoreNetworkChangeValues
newCoreNetworkChangeValues =
  CoreNetworkChangeValues'
    { sharedSegments =
        Prelude.Nothing,
      cidr = Prelude.Nothing,
      asn = Prelude.Nothing,
      segmentName = Prelude.Nothing,
      edgeLocations = Prelude.Nothing,
      insideCidrBlocks = Prelude.Nothing,
      destinationIdentifier = Prelude.Nothing
    }

-- | The shared segments for a core network change value.
coreNetworkChangeValues_sharedSegments :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe [Prelude.Text])
coreNetworkChangeValues_sharedSegments = Lens.lens (\CoreNetworkChangeValues' {sharedSegments} -> sharedSegments) (\s@CoreNetworkChangeValues' {} a -> s {sharedSegments = a} :: CoreNetworkChangeValues) Prelude.. Lens.mapping Lens.coerced

-- | The IP addresses used for a core network.
coreNetworkChangeValues_cidr :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeValues_cidr = Lens.lens (\CoreNetworkChangeValues' {cidr} -> cidr) (\s@CoreNetworkChangeValues' {} a -> s {cidr = a} :: CoreNetworkChangeValues)

-- | The ASN of a core network.
coreNetworkChangeValues_asn :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe Prelude.Integer)
coreNetworkChangeValues_asn = Lens.lens (\CoreNetworkChangeValues' {asn} -> asn) (\s@CoreNetworkChangeValues' {} a -> s {asn = a} :: CoreNetworkChangeValues)

-- | The names of the segments in a core network.
coreNetworkChangeValues_segmentName :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeValues_segmentName = Lens.lens (\CoreNetworkChangeValues' {segmentName} -> segmentName) (\s@CoreNetworkChangeValues' {} a -> s {segmentName = a} :: CoreNetworkChangeValues)

-- | The Regions where edges are located in a core network.
coreNetworkChangeValues_edgeLocations :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe [Prelude.Text])
coreNetworkChangeValues_edgeLocations = Lens.lens (\CoreNetworkChangeValues' {edgeLocations} -> edgeLocations) (\s@CoreNetworkChangeValues' {} a -> s {edgeLocations = a} :: CoreNetworkChangeValues) Prelude.. Lens.mapping Lens.coerced

-- | The inside IP addresses used for core network change values.
coreNetworkChangeValues_insideCidrBlocks :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe [Prelude.Text])
coreNetworkChangeValues_insideCidrBlocks = Lens.lens (\CoreNetworkChangeValues' {insideCidrBlocks} -> insideCidrBlocks) (\s@CoreNetworkChangeValues' {} a -> s {insideCidrBlocks = a} :: CoreNetworkChangeValues) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the destination.
coreNetworkChangeValues_destinationIdentifier :: Lens.Lens' CoreNetworkChangeValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeValues_destinationIdentifier = Lens.lens (\CoreNetworkChangeValues' {destinationIdentifier} -> destinationIdentifier) (\s@CoreNetworkChangeValues' {} a -> s {destinationIdentifier = a} :: CoreNetworkChangeValues)

instance Core.FromJSON CoreNetworkChangeValues where
  parseJSON =
    Core.withObject
      "CoreNetworkChangeValues"
      ( \x ->
          CoreNetworkChangeValues'
            Prelude.<$> (x Core..:? "SharedSegments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Cidr")
            Prelude.<*> (x Core..:? "Asn")
            Prelude.<*> (x Core..:? "SegmentName")
            Prelude.<*> (x Core..:? "EdgeLocations" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "InsideCidrBlocks"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DestinationIdentifier")
      )

instance Prelude.Hashable CoreNetworkChangeValues where
  hashWithSalt _salt CoreNetworkChangeValues' {..} =
    _salt `Prelude.hashWithSalt` sharedSegments
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` segmentName
      `Prelude.hashWithSalt` edgeLocations
      `Prelude.hashWithSalt` insideCidrBlocks
      `Prelude.hashWithSalt` destinationIdentifier

instance Prelude.NFData CoreNetworkChangeValues where
  rnf CoreNetworkChangeValues' {..} =
    Prelude.rnf sharedSegments
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf segmentName
      `Prelude.seq` Prelude.rnf edgeLocations
      `Prelude.seq` Prelude.rnf insideCidrBlocks
      `Prelude.seq` Prelude.rnf destinationIdentifier
