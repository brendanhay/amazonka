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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkEdge
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkEdge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network edge.
--
-- /See:/ 'newCoreNetworkEdge' smart constructor.
data CoreNetworkEdge = CoreNetworkEdge'
  { -- | The Region where a core network edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ASN of a core network edge.
    asn :: Prelude.Maybe Prelude.Integer,
    -- | The inside IP addresses used for core network edges.
    insideCidrBlocks :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkEdge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeLocation', 'coreNetworkEdge_edgeLocation' - The Region where a core network edge is located.
--
-- 'asn', 'coreNetworkEdge_asn' - The ASN of a core network edge.
--
-- 'insideCidrBlocks', 'coreNetworkEdge_insideCidrBlocks' - The inside IP addresses used for core network edges.
newCoreNetworkEdge ::
  CoreNetworkEdge
newCoreNetworkEdge =
  CoreNetworkEdge'
    { edgeLocation = Prelude.Nothing,
      asn = Prelude.Nothing,
      insideCidrBlocks = Prelude.Nothing
    }

-- | The Region where a core network edge is located.
coreNetworkEdge_edgeLocation :: Lens.Lens' CoreNetworkEdge (Prelude.Maybe Prelude.Text)
coreNetworkEdge_edgeLocation = Lens.lens (\CoreNetworkEdge' {edgeLocation} -> edgeLocation) (\s@CoreNetworkEdge' {} a -> s {edgeLocation = a} :: CoreNetworkEdge)

-- | The ASN of a core network edge.
coreNetworkEdge_asn :: Lens.Lens' CoreNetworkEdge (Prelude.Maybe Prelude.Integer)
coreNetworkEdge_asn = Lens.lens (\CoreNetworkEdge' {asn} -> asn) (\s@CoreNetworkEdge' {} a -> s {asn = a} :: CoreNetworkEdge)

-- | The inside IP addresses used for core network edges.
coreNetworkEdge_insideCidrBlocks :: Lens.Lens' CoreNetworkEdge (Prelude.Maybe [Prelude.Text])
coreNetworkEdge_insideCidrBlocks = Lens.lens (\CoreNetworkEdge' {insideCidrBlocks} -> insideCidrBlocks) (\s@CoreNetworkEdge' {} a -> s {insideCidrBlocks = a} :: CoreNetworkEdge) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CoreNetworkEdge where
  parseJSON =
    Data.withObject
      "CoreNetworkEdge"
      ( \x ->
          CoreNetworkEdge'
            Prelude.<$> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "Asn")
            Prelude.<*> ( x Data..:? "InsideCidrBlocks"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CoreNetworkEdge where
  hashWithSalt _salt CoreNetworkEdge' {..} =
    _salt `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` insideCidrBlocks

instance Prelude.NFData CoreNetworkEdge where
  rnf CoreNetworkEdge' {..} =
    Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf insideCidrBlocks
