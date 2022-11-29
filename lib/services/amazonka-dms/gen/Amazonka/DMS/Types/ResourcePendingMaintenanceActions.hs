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
-- Module      : Amazonka.DMS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ResourcePendingMaintenanceActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.PendingMaintenanceAction
import qualified Amazonka.Prelude as Prelude

-- | Identifies an DMS resource and any pending actions for it.
--
-- /See:/ 'newResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { -- | The Amazon Resource Name (ARN) of the DMS resource that the pending
    -- maintenance action applies to. For information about creating an ARN,
    -- see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for DMS>
    -- in the DMS documentation.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the pending maintenance action.
    pendingMaintenanceActionDetails :: Prelude.Maybe [PendingMaintenanceAction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePendingMaintenanceActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'resourcePendingMaintenanceActions_resourceIdentifier' - The Amazon Resource Name (ARN) of the DMS resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for DMS>
-- in the DMS documentation.
--
-- 'pendingMaintenanceActionDetails', 'resourcePendingMaintenanceActions_pendingMaintenanceActionDetails' - Detailed information about the pending maintenance action.
newResourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
newResourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { resourceIdentifier =
        Prelude.Nothing,
      pendingMaintenanceActionDetails =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the DMS resource that the pending
-- maintenance action applies to. For information about creating an ARN,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for DMS>
-- in the DMS documentation.
resourcePendingMaintenanceActions_resourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Prelude.Maybe Prelude.Text)
resourcePendingMaintenanceActions_resourceIdentifier = Lens.lens (\ResourcePendingMaintenanceActions' {resourceIdentifier} -> resourceIdentifier) (\s@ResourcePendingMaintenanceActions' {} a -> s {resourceIdentifier = a} :: ResourcePendingMaintenanceActions)

-- | Detailed information about the pending maintenance action.
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Prelude.Maybe [PendingMaintenanceAction])
resourcePendingMaintenanceActions_pendingMaintenanceActionDetails = Lens.lens (\ResourcePendingMaintenanceActions' {pendingMaintenanceActionDetails} -> pendingMaintenanceActionDetails) (\s@ResourcePendingMaintenanceActions' {} a -> s {pendingMaintenanceActionDetails = a} :: ResourcePendingMaintenanceActions) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    ResourcePendingMaintenanceActions
  where
  parseJSON =
    Core.withObject
      "ResourcePendingMaintenanceActions"
      ( \x ->
          ResourcePendingMaintenanceActions'
            Prelude.<$> (x Core..:? "ResourceIdentifier")
            Prelude.<*> ( x Core..:? "PendingMaintenanceActionDetails"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ResourcePendingMaintenanceActions
  where
  hashWithSalt
    _salt
    ResourcePendingMaintenanceActions' {..} =
      _salt `Prelude.hashWithSalt` resourceIdentifier
        `Prelude.hashWithSalt` pendingMaintenanceActionDetails

instance
  Prelude.NFData
    ResourcePendingMaintenanceActions
  where
  rnf ResourcePendingMaintenanceActions' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf pendingMaintenanceActionDetails
