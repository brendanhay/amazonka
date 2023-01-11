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
-- Module      : Amazonka.SecurityHub.Types.NetworkHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NetworkHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.NetworkPathComponentDetails

-- | Details about a network path component that occurs before or after the
-- current component.
--
-- /See:/ 'newNetworkHeader' smart constructor.
data NetworkHeader = NetworkHeader'
  { -- | Information about the destination of the component.
    destination :: Prelude.Maybe NetworkPathComponentDetails,
    -- | The protocol used for the component.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | Information about the origin of the component.
    source :: Prelude.Maybe NetworkPathComponentDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'networkHeader_destination' - Information about the destination of the component.
--
-- 'protocol', 'networkHeader_protocol' - The protocol used for the component.
--
-- 'source', 'networkHeader_source' - Information about the origin of the component.
newNetworkHeader ::
  NetworkHeader
newNetworkHeader =
  NetworkHeader'
    { destination = Prelude.Nothing,
      protocol = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | Information about the destination of the component.
networkHeader_destination :: Lens.Lens' NetworkHeader (Prelude.Maybe NetworkPathComponentDetails)
networkHeader_destination = Lens.lens (\NetworkHeader' {destination} -> destination) (\s@NetworkHeader' {} a -> s {destination = a} :: NetworkHeader)

-- | The protocol used for the component.
networkHeader_protocol :: Lens.Lens' NetworkHeader (Prelude.Maybe Prelude.Text)
networkHeader_protocol = Lens.lens (\NetworkHeader' {protocol} -> protocol) (\s@NetworkHeader' {} a -> s {protocol = a} :: NetworkHeader)

-- | Information about the origin of the component.
networkHeader_source :: Lens.Lens' NetworkHeader (Prelude.Maybe NetworkPathComponentDetails)
networkHeader_source = Lens.lens (\NetworkHeader' {source} -> source) (\s@NetworkHeader' {} a -> s {source = a} :: NetworkHeader)

instance Data.FromJSON NetworkHeader where
  parseJSON =
    Data.withObject
      "NetworkHeader"
      ( \x ->
          NetworkHeader'
            Prelude.<$> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "Protocol")
            Prelude.<*> (x Data..:? "Source")
      )

instance Prelude.Hashable NetworkHeader where
  hashWithSalt _salt NetworkHeader' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` source

instance Prelude.NFData NetworkHeader where
  rnf NetworkHeader' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf source

instance Data.ToJSON NetworkHeader where
  toJSON NetworkHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destination" Data..=) Prelude.<$> destination,
            ("Protocol" Data..=) Prelude.<$> protocol,
            ("Source" Data..=) Prelude.<$> source
          ]
      )
