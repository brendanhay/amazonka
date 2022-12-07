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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Availability Zone for a subnet in a subnet group.
--
-- /See:/ 'newAwsRdsDbSubnetGroupSubnetAvailabilityZone' smart constructor.
data AwsRdsDbSubnetGroupSubnetAvailabilityZone = AwsRdsDbSubnetGroupSubnetAvailabilityZone'
  { -- | The name of the Availability Zone for a subnet in the subnet group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSubnetGroupSubnetAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsRdsDbSubnetGroupSubnetAvailabilityZone_name' - The name of the Availability Zone for a subnet in the subnet group.
newAwsRdsDbSubnetGroupSubnetAvailabilityZone ::
  AwsRdsDbSubnetGroupSubnetAvailabilityZone
newAwsRdsDbSubnetGroupSubnetAvailabilityZone =
  AwsRdsDbSubnetGroupSubnetAvailabilityZone'
    { name =
        Prelude.Nothing
    }

-- | The name of the Availability Zone for a subnet in the subnet group.
awsRdsDbSubnetGroupSubnetAvailabilityZone_name :: Lens.Lens' AwsRdsDbSubnetGroupSubnetAvailabilityZone (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnetAvailabilityZone_name = Lens.lens (\AwsRdsDbSubnetGroupSubnetAvailabilityZone' {name} -> name) (\s@AwsRdsDbSubnetGroupSubnetAvailabilityZone' {} a -> s {name = a} :: AwsRdsDbSubnetGroupSubnetAvailabilityZone)

instance
  Data.FromJSON
    AwsRdsDbSubnetGroupSubnetAvailabilityZone
  where
  parseJSON =
    Data.withObject
      "AwsRdsDbSubnetGroupSubnetAvailabilityZone"
      ( \x ->
          AwsRdsDbSubnetGroupSubnetAvailabilityZone'
            Prelude.<$> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    AwsRdsDbSubnetGroupSubnetAvailabilityZone
  where
  hashWithSalt
    _salt
    AwsRdsDbSubnetGroupSubnetAvailabilityZone' {..} =
      _salt `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    AwsRdsDbSubnetGroupSubnetAvailabilityZone
  where
  rnf AwsRdsDbSubnetGroupSubnetAvailabilityZone' {..} =
    Prelude.rnf name

instance
  Data.ToJSON
    AwsRdsDbSubnetGroupSubnetAvailabilityZone
  where
  toJSON AwsRdsDbSubnetGroupSubnetAvailabilityZone' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )
