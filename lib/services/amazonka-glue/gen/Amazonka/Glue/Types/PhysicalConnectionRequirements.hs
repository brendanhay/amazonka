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
-- Module      : Amazonka.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PhysicalConnectionRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the physical requirements for a connection.
--
-- /See:/ 'newPhysicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { -- | The connection\'s Availability Zone. This field is redundant because the
    -- specified subnet implies the Availability Zone to be used. Currently the
    -- field must be populated, but it will be deprecated in the future.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The security group ID list used by the connection.
    securityGroupIdList :: Prelude.Maybe [Prelude.Text],
    -- | The subnet ID used by the connection.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalConnectionRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'physicalConnectionRequirements_availabilityZone' - The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
--
-- 'securityGroupIdList', 'physicalConnectionRequirements_securityGroupIdList' - The security group ID list used by the connection.
--
-- 'subnetId', 'physicalConnectionRequirements_subnetId' - The subnet ID used by the connection.
newPhysicalConnectionRequirements ::
  PhysicalConnectionRequirements
newPhysicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { availabilityZone =
        Prelude.Nothing,
      securityGroupIdList = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
physicalConnectionRequirements_availabilityZone :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe Prelude.Text)
physicalConnectionRequirements_availabilityZone = Lens.lens (\PhysicalConnectionRequirements' {availabilityZone} -> availabilityZone) (\s@PhysicalConnectionRequirements' {} a -> s {availabilityZone = a} :: PhysicalConnectionRequirements)

-- | The security group ID list used by the connection.
physicalConnectionRequirements_securityGroupIdList :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe [Prelude.Text])
physicalConnectionRequirements_securityGroupIdList = Lens.lens (\PhysicalConnectionRequirements' {securityGroupIdList} -> securityGroupIdList) (\s@PhysicalConnectionRequirements' {} a -> s {securityGroupIdList = a} :: PhysicalConnectionRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID used by the connection.
physicalConnectionRequirements_subnetId :: Lens.Lens' PhysicalConnectionRequirements (Prelude.Maybe Prelude.Text)
physicalConnectionRequirements_subnetId = Lens.lens (\PhysicalConnectionRequirements' {subnetId} -> subnetId) (\s@PhysicalConnectionRequirements' {} a -> s {subnetId = a} :: PhysicalConnectionRequirements)

instance Data.FromJSON PhysicalConnectionRequirements where
  parseJSON =
    Data.withObject
      "PhysicalConnectionRequirements"
      ( \x ->
          PhysicalConnectionRequirements'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> ( x
                            Data..:? "SecurityGroupIdList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetId")
      )

instance
  Prelude.Hashable
    PhysicalConnectionRequirements
  where
  hashWithSalt
    _salt
    PhysicalConnectionRequirements' {..} =
      _salt
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` securityGroupIdList
        `Prelude.hashWithSalt` subnetId

instance
  Prelude.NFData
    PhysicalConnectionRequirements
  where
  rnf PhysicalConnectionRequirements' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf securityGroupIdList `Prelude.seq`
        Prelude.rnf subnetId

instance Data.ToJSON PhysicalConnectionRequirements where
  toJSON PhysicalConnectionRequirements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("SecurityGroupIdList" Data..=)
              Prelude.<$> securityGroupIdList,
            ("SubnetId" Data..=) Prelude.<$> subnetId
          ]
      )
