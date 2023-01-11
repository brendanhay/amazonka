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
-- Module      : Amazonka.EC2.Types.AnalysisPacketHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AnalysisPacketHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Describes a header. Reflects any changes made by a component as traffic
-- passes through. The fields of an inbound header are null except for the
-- first component of a path.
--
-- /See:/ 'newAnalysisPacketHeader' smart constructor.
data AnalysisPacketHeader = AnalysisPacketHeader'
  { -- | The destination addresses.
    destinationAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The destination port ranges.
    destinationPortRanges :: Prelude.Maybe [PortRange],
    -- | The protocol.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The source addresses.
    sourceAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The source port ranges.
    sourcePortRanges :: Prelude.Maybe [PortRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisPacketHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationAddresses', 'analysisPacketHeader_destinationAddresses' - The destination addresses.
--
-- 'destinationPortRanges', 'analysisPacketHeader_destinationPortRanges' - The destination port ranges.
--
-- 'protocol', 'analysisPacketHeader_protocol' - The protocol.
--
-- 'sourceAddresses', 'analysisPacketHeader_sourceAddresses' - The source addresses.
--
-- 'sourcePortRanges', 'analysisPacketHeader_sourcePortRanges' - The source port ranges.
newAnalysisPacketHeader ::
  AnalysisPacketHeader
newAnalysisPacketHeader =
  AnalysisPacketHeader'
    { destinationAddresses =
        Prelude.Nothing,
      destinationPortRanges = Prelude.Nothing,
      protocol = Prelude.Nothing,
      sourceAddresses = Prelude.Nothing,
      sourcePortRanges = Prelude.Nothing
    }

-- | The destination addresses.
analysisPacketHeader_destinationAddresses :: Lens.Lens' AnalysisPacketHeader (Prelude.Maybe [Prelude.Text])
analysisPacketHeader_destinationAddresses = Lens.lens (\AnalysisPacketHeader' {destinationAddresses} -> destinationAddresses) (\s@AnalysisPacketHeader' {} a -> s {destinationAddresses = a} :: AnalysisPacketHeader) Prelude.. Lens.mapping Lens.coerced

-- | The destination port ranges.
analysisPacketHeader_destinationPortRanges :: Lens.Lens' AnalysisPacketHeader (Prelude.Maybe [PortRange])
analysisPacketHeader_destinationPortRanges = Lens.lens (\AnalysisPacketHeader' {destinationPortRanges} -> destinationPortRanges) (\s@AnalysisPacketHeader' {} a -> s {destinationPortRanges = a} :: AnalysisPacketHeader) Prelude.. Lens.mapping Lens.coerced

-- | The protocol.
analysisPacketHeader_protocol :: Lens.Lens' AnalysisPacketHeader (Prelude.Maybe Prelude.Text)
analysisPacketHeader_protocol = Lens.lens (\AnalysisPacketHeader' {protocol} -> protocol) (\s@AnalysisPacketHeader' {} a -> s {protocol = a} :: AnalysisPacketHeader)

-- | The source addresses.
analysisPacketHeader_sourceAddresses :: Lens.Lens' AnalysisPacketHeader (Prelude.Maybe [Prelude.Text])
analysisPacketHeader_sourceAddresses = Lens.lens (\AnalysisPacketHeader' {sourceAddresses} -> sourceAddresses) (\s@AnalysisPacketHeader' {} a -> s {sourceAddresses = a} :: AnalysisPacketHeader) Prelude.. Lens.mapping Lens.coerced

-- | The source port ranges.
analysisPacketHeader_sourcePortRanges :: Lens.Lens' AnalysisPacketHeader (Prelude.Maybe [PortRange])
analysisPacketHeader_sourcePortRanges = Lens.lens (\AnalysisPacketHeader' {sourcePortRanges} -> sourcePortRanges) (\s@AnalysisPacketHeader' {} a -> s {sourcePortRanges = a} :: AnalysisPacketHeader) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML AnalysisPacketHeader where
  parseXML x =
    AnalysisPacketHeader'
      Prelude.<$> ( x Data..@? "destinationAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "destinationPortRangeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "protocol")
      Prelude.<*> ( x Data..@? "sourceAddressSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "sourcePortRangeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable AnalysisPacketHeader where
  hashWithSalt _salt AnalysisPacketHeader' {..} =
    _salt `Prelude.hashWithSalt` destinationAddresses
      `Prelude.hashWithSalt` destinationPortRanges
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` sourceAddresses
      `Prelude.hashWithSalt` sourcePortRanges

instance Prelude.NFData AnalysisPacketHeader where
  rnf AnalysisPacketHeader' {..} =
    Prelude.rnf destinationAddresses
      `Prelude.seq` Prelude.rnf destinationPortRanges
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf sourceAddresses
      `Prelude.seq` Prelude.rnf sourcePortRanges
