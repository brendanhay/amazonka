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
-- Module      : Amazonka.VPCLattice.Types.ServiceNetworkSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ServiceNetworkSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a service network.
--
-- /See:/ 'newServiceNetworkSummary' smart constructor.
data ServiceNetworkSummary = ServiceNetworkSummary'
  { -- | The Amazon Resource Name (ARN) of the service network.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service network was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the service network.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service network was last updated, specified
    -- in ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the service network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of services associated with the service network.
    numberOfAssociatedServices :: Prelude.Maybe Prelude.Integer,
    -- | The number of VPCs associated with the service network.
    numberOfAssociatedVPCs :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNetworkSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'serviceNetworkSummary_arn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'createdAt', 'serviceNetworkSummary_createdAt' - The date and time that the service network was created, specified in
-- ISO-8601 format.
--
-- 'id', 'serviceNetworkSummary_id' - The ID of the service network.
--
-- 'lastUpdatedAt', 'serviceNetworkSummary_lastUpdatedAt' - The date and time that the service network was last updated, specified
-- in ISO-8601 format.
--
-- 'name', 'serviceNetworkSummary_name' - The name of the service network.
--
-- 'numberOfAssociatedServices', 'serviceNetworkSummary_numberOfAssociatedServices' - The number of services associated with the service network.
--
-- 'numberOfAssociatedVPCs', 'serviceNetworkSummary_numberOfAssociatedVPCs' - The number of VPCs associated with the service network.
newServiceNetworkSummary ::
  ServiceNetworkSummary
newServiceNetworkSummary =
  ServiceNetworkSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfAssociatedServices = Prelude.Nothing,
      numberOfAssociatedVPCs = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service network.
serviceNetworkSummary_arn :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.Text)
serviceNetworkSummary_arn = Lens.lens (\ServiceNetworkSummary' {arn} -> arn) (\s@ServiceNetworkSummary' {} a -> s {arn = a} :: ServiceNetworkSummary)

-- | The date and time that the service network was created, specified in
-- ISO-8601 format.
serviceNetworkSummary_createdAt :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.UTCTime)
serviceNetworkSummary_createdAt = Lens.lens (\ServiceNetworkSummary' {createdAt} -> createdAt) (\s@ServiceNetworkSummary' {} a -> s {createdAt = a} :: ServiceNetworkSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the service network.
serviceNetworkSummary_id :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.Text)
serviceNetworkSummary_id = Lens.lens (\ServiceNetworkSummary' {id} -> id) (\s@ServiceNetworkSummary' {} a -> s {id = a} :: ServiceNetworkSummary)

-- | The date and time that the service network was last updated, specified
-- in ISO-8601 format.
serviceNetworkSummary_lastUpdatedAt :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.UTCTime)
serviceNetworkSummary_lastUpdatedAt = Lens.lens (\ServiceNetworkSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@ServiceNetworkSummary' {} a -> s {lastUpdatedAt = a} :: ServiceNetworkSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the service network.
serviceNetworkSummary_name :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.Text)
serviceNetworkSummary_name = Lens.lens (\ServiceNetworkSummary' {name} -> name) (\s@ServiceNetworkSummary' {} a -> s {name = a} :: ServiceNetworkSummary)

-- | The number of services associated with the service network.
serviceNetworkSummary_numberOfAssociatedServices :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.Integer)
serviceNetworkSummary_numberOfAssociatedServices = Lens.lens (\ServiceNetworkSummary' {numberOfAssociatedServices} -> numberOfAssociatedServices) (\s@ServiceNetworkSummary' {} a -> s {numberOfAssociatedServices = a} :: ServiceNetworkSummary)

-- | The number of VPCs associated with the service network.
serviceNetworkSummary_numberOfAssociatedVPCs :: Lens.Lens' ServiceNetworkSummary (Prelude.Maybe Prelude.Integer)
serviceNetworkSummary_numberOfAssociatedVPCs = Lens.lens (\ServiceNetworkSummary' {numberOfAssociatedVPCs} -> numberOfAssociatedVPCs) (\s@ServiceNetworkSummary' {} a -> s {numberOfAssociatedVPCs = a} :: ServiceNetworkSummary)

instance Data.FromJSON ServiceNetworkSummary where
  parseJSON =
    Data.withObject
      "ServiceNetworkSummary"
      ( \x ->
          ServiceNetworkSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "numberOfAssociatedServices")
            Prelude.<*> (x Data..:? "numberOfAssociatedVPCs")
      )

instance Prelude.Hashable ServiceNetworkSummary where
  hashWithSalt _salt ServiceNetworkSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfAssociatedServices
      `Prelude.hashWithSalt` numberOfAssociatedVPCs

instance Prelude.NFData ServiceNetworkSummary where
  rnf ServiceNetworkSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfAssociatedServices
      `Prelude.seq` Prelude.rnf numberOfAssociatedVPCs
