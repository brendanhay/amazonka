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
-- Module      : Network.AWS.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PhysicalConnectionRequirements where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the physical requirements for a connection.
--
-- /See:/ 'newPhysicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { -- | The security group ID list used by the connection.
    securityGroupIdList :: Core.Maybe [Core.Text],
    -- | The connection\'s Availability Zone. This field is redundant because the
    -- specified subnet implies the Availability Zone to be used. Currently the
    -- field must be populated, but it will be deprecated in the future.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The subnet ID used by the connection.
    subnetId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PhysicalConnectionRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIdList', 'physicalConnectionRequirements_securityGroupIdList' - The security group ID list used by the connection.
--
-- 'availabilityZone', 'physicalConnectionRequirements_availabilityZone' - The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
--
-- 'subnetId', 'physicalConnectionRequirements_subnetId' - The subnet ID used by the connection.
newPhysicalConnectionRequirements ::
  PhysicalConnectionRequirements
newPhysicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { securityGroupIdList =
        Core.Nothing,
      availabilityZone = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | The security group ID list used by the connection.
physicalConnectionRequirements_securityGroupIdList :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe [Core.Text])
physicalConnectionRequirements_securityGroupIdList = Lens.lens (\PhysicalConnectionRequirements' {securityGroupIdList} -> securityGroupIdList) (\s@PhysicalConnectionRequirements' {} a -> s {securityGroupIdList = a} :: PhysicalConnectionRequirements) Core.. Lens.mapping Lens._Coerce

-- | The connection\'s Availability Zone. This field is redundant because the
-- specified subnet implies the Availability Zone to be used. Currently the
-- field must be populated, but it will be deprecated in the future.
physicalConnectionRequirements_availabilityZone :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe Core.Text)
physicalConnectionRequirements_availabilityZone = Lens.lens (\PhysicalConnectionRequirements' {availabilityZone} -> availabilityZone) (\s@PhysicalConnectionRequirements' {} a -> s {availabilityZone = a} :: PhysicalConnectionRequirements)

-- | The subnet ID used by the connection.
physicalConnectionRequirements_subnetId :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe Core.Text)
physicalConnectionRequirements_subnetId = Lens.lens (\PhysicalConnectionRequirements' {subnetId} -> subnetId) (\s@PhysicalConnectionRequirements' {} a -> s {subnetId = a} :: PhysicalConnectionRequirements)

instance Core.FromJSON PhysicalConnectionRequirements where
  parseJSON =
    Core.withObject
      "PhysicalConnectionRequirements"
      ( \x ->
          PhysicalConnectionRequirements'
            Core.<$> ( x Core..:? "SecurityGroupIdList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "SubnetId")
      )

instance Core.Hashable PhysicalConnectionRequirements

instance Core.NFData PhysicalConnectionRequirements

instance Core.ToJSON PhysicalConnectionRequirements where
  toJSON PhysicalConnectionRequirements' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityGroupIdList" Core..=)
              Core.<$> securityGroupIdList,
            ("AvailabilityZone" Core..=)
              Core.<$> availabilityZone,
            ("SubnetId" Core..=) Core.<$> subnetId
          ]
      )
