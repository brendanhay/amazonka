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
-- Module      : Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ResourcePendingMaintenanceActions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.PendingMaintenanceAction

-- | Describes the pending maintenance actions for a resource.
--
-- /See:/ 'newResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { -- | A list that provides details about the pending maintenance actions for
    -- the resource.
    pendingMaintenanceActionDetails :: Core.Maybe [PendingMaintenanceAction],
    -- | The ARN of the resource that has pending maintenance actions.
    resourceIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourcePendingMaintenanceActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingMaintenanceActionDetails', 'resourcePendingMaintenanceActions_pendingMaintenanceActionDetails' - A list that provides details about the pending maintenance actions for
-- the resource.
--
-- 'resourceIdentifier', 'resourcePendingMaintenanceActions_resourceIdentifier' - The ARN of the resource that has pending maintenance actions.
newResourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
newResourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { pendingMaintenanceActionDetails =
        Core.Nothing,
      resourceIdentifier = Core.Nothing
    }

-- | A list that provides details about the pending maintenance actions for
-- the resource.
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe [PendingMaintenanceAction])
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails = Lens.lens (\ResourcePendingMaintenanceActions' {pendingMaintenanceActionDetails} -> pendingMaintenanceActionDetails) (\s@ResourcePendingMaintenanceActions' {} a -> s {pendingMaintenanceActionDetails = a} :: ResourcePendingMaintenanceActions) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the resource that has pending maintenance actions.
resourcePendingMaintenanceActions_resourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe Core.Text)
resourcePendingMaintenanceActions_resourceIdentifier = Lens.lens (\ResourcePendingMaintenanceActions' {resourceIdentifier} -> resourceIdentifier) (\s@ResourcePendingMaintenanceActions' {} a -> s {resourceIdentifier = a} :: ResourcePendingMaintenanceActions)

instance
  Core.FromXML
    ResourcePendingMaintenanceActions
  where
  parseXML x =
    ResourcePendingMaintenanceActions'
      Core.<$> ( x Core..@? "PendingMaintenanceActionDetails"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "PendingMaintenanceAction")
               )
      Core.<*> (x Core..@? "ResourceIdentifier")

instance
  Core.Hashable
    ResourcePendingMaintenanceActions

instance
  Core.NFData
    ResourcePendingMaintenanceActions
