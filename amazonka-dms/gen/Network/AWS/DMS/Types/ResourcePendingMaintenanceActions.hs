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
-- Module      : Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ResourcePendingMaintenanceActions where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.PendingMaintenanceAction
import qualified Network.AWS.Lens as Lens

-- | Identifies an AWS DMS resource and any pending actions for it.
--
-- /See:/ 'newResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { -- | Detailed information about the pending maintenance action.
    pendingMaintenanceActionDetails :: Core.Maybe [PendingMaintenanceAction],
    -- | The Amazon Resource Name (ARN) of the DMS resource that the pending
    -- maintenance action applies to. For information about creating an ARN,
    -- see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS>
    -- in the DMS documentation.
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
-- 'pendingMaintenanceActionDetails', 'resourcePendingMaintenanceActions_pendingMaintenanceActionDetails' - Detailed information about the pending maintenance action.
--
-- 'resourceIdentifier', 'resourcePendingMaintenanceActions_resourceIdentifier' - The Amazon Resource Name (ARN) of the DMS resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS>
-- in the DMS documentation.
newResourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
newResourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { pendingMaintenanceActionDetails =
        Core.Nothing,
      resourceIdentifier = Core.Nothing
    }

-- | Detailed information about the pending maintenance action.
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe [PendingMaintenanceAction])
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails = Lens.lens (\ResourcePendingMaintenanceActions' {pendingMaintenanceActionDetails} -> pendingMaintenanceActionDetails) (\s@ResourcePendingMaintenanceActions' {} a -> s {pendingMaintenanceActionDetails = a} :: ResourcePendingMaintenanceActions) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the DMS resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS>
-- in the DMS documentation.
resourcePendingMaintenanceActions_resourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe Core.Text)
resourcePendingMaintenanceActions_resourceIdentifier = Lens.lens (\ResourcePendingMaintenanceActions' {resourceIdentifier} -> resourceIdentifier) (\s@ResourcePendingMaintenanceActions' {} a -> s {resourceIdentifier = a} :: ResourcePendingMaintenanceActions)

instance
  Core.FromJSON
    ResourcePendingMaintenanceActions
  where
  parseJSON =
    Core.withObject
      "ResourcePendingMaintenanceActions"
      ( \x ->
          ResourcePendingMaintenanceActions'
            Core.<$> ( x Core..:? "PendingMaintenanceActionDetails"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ResourceIdentifier")
      )

instance
  Core.Hashable
    ResourcePendingMaintenanceActions

instance
  Core.NFData
    ResourcePendingMaintenanceActions
