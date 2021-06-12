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
-- Module      : Network.AWS.SSM.Types.AssociationOverview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationOverview where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the association.
--
-- /See:/ 'newAssociationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
  { -- | The status of the association. Status can be: Pending, Success, or
    -- Failed.
    status :: Core.Maybe Core.Text,
    -- | A detailed status of the association.
    detailedStatus :: Core.Maybe Core.Text,
    -- | Returns the number of targets for the association status. For example,
    -- if you created an association with two instances, and one of them was
    -- successful, this would return the count of instances by status.
    associationStatusAggregatedCount :: Core.Maybe (Core.HashMap Core.Text Core.Int)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociationOverview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'associationOverview_status' - The status of the association. Status can be: Pending, Success, or
-- Failed.
--
-- 'detailedStatus', 'associationOverview_detailedStatus' - A detailed status of the association.
--
-- 'associationStatusAggregatedCount', 'associationOverview_associationStatusAggregatedCount' - Returns the number of targets for the association status. For example,
-- if you created an association with two instances, and one of them was
-- successful, this would return the count of instances by status.
newAssociationOverview ::
  AssociationOverview
newAssociationOverview =
  AssociationOverview'
    { status = Core.Nothing,
      detailedStatus = Core.Nothing,
      associationStatusAggregatedCount = Core.Nothing
    }

-- | The status of the association. Status can be: Pending, Success, or
-- Failed.
associationOverview_status :: Lens.Lens' AssociationOverview (Core.Maybe Core.Text)
associationOverview_status = Lens.lens (\AssociationOverview' {status} -> status) (\s@AssociationOverview' {} a -> s {status = a} :: AssociationOverview)

-- | A detailed status of the association.
associationOverview_detailedStatus :: Lens.Lens' AssociationOverview (Core.Maybe Core.Text)
associationOverview_detailedStatus = Lens.lens (\AssociationOverview' {detailedStatus} -> detailedStatus) (\s@AssociationOverview' {} a -> s {detailedStatus = a} :: AssociationOverview)

-- | Returns the number of targets for the association status. For example,
-- if you created an association with two instances, and one of them was
-- successful, this would return the count of instances by status.
associationOverview_associationStatusAggregatedCount :: Lens.Lens' AssociationOverview (Core.Maybe (Core.HashMap Core.Text Core.Int))
associationOverview_associationStatusAggregatedCount = Lens.lens (\AssociationOverview' {associationStatusAggregatedCount} -> associationStatusAggregatedCount) (\s@AssociationOverview' {} a -> s {associationStatusAggregatedCount = a} :: AssociationOverview) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AssociationOverview where
  parseJSON =
    Core.withObject
      "AssociationOverview"
      ( \x ->
          AssociationOverview'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "DetailedStatus")
            Core.<*> ( x Core..:? "AssociationStatusAggregatedCount"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable AssociationOverview

instance Core.NFData AssociationOverview
