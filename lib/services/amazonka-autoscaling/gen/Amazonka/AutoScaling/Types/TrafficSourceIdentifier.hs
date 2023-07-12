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
-- Module      : Amazonka.AutoScaling.Types.TrafficSourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TrafficSourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the identifier of a traffic source.
--
-- Currently, you must specify an Amazon Resource Name (ARN) for an
-- existing VPC Lattice target group.
--
-- /See:/ 'newTrafficSourceIdentifier' smart constructor.
data TrafficSourceIdentifier = TrafficSourceIdentifier'
  { -- | The unique identifier of the traffic source.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficSourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'trafficSourceIdentifier_identifier' - The unique identifier of the traffic source.
newTrafficSourceIdentifier ::
  TrafficSourceIdentifier
newTrafficSourceIdentifier =
  TrafficSourceIdentifier'
    { identifier =
        Prelude.Nothing
    }

-- | The unique identifier of the traffic source.
trafficSourceIdentifier_identifier :: Lens.Lens' TrafficSourceIdentifier (Prelude.Maybe Prelude.Text)
trafficSourceIdentifier_identifier = Lens.lens (\TrafficSourceIdentifier' {identifier} -> identifier) (\s@TrafficSourceIdentifier' {} a -> s {identifier = a} :: TrafficSourceIdentifier)

instance Data.FromXML TrafficSourceIdentifier where
  parseXML x =
    TrafficSourceIdentifier'
      Prelude.<$> (x Data..@? "Identifier")

instance Prelude.Hashable TrafficSourceIdentifier where
  hashWithSalt _salt TrafficSourceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData TrafficSourceIdentifier where
  rnf TrafficSourceIdentifier' {..} =
    Prelude.rnf identifier

instance Data.ToQuery TrafficSourceIdentifier where
  toQuery TrafficSourceIdentifier' {..} =
    Prelude.mconcat ["Identifier" Data.=: identifier]
