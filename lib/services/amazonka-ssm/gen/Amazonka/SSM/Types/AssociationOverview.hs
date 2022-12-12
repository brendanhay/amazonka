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
-- Module      : Amazonka.SSM.Types.AssociationOverview
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationOverview where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the association.
--
-- /See:/ 'newAssociationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
  { -- | Returns the number of targets for the association status. For example,
    -- if you created an association with two managed nodes, and one of them
    -- was successful, this would return the count of managed nodes by status.
    associationStatusAggregatedCount :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | A detailed status of the association.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The status of the association. Status can be: Pending, Success, or
    -- Failed.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationOverview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationStatusAggregatedCount', 'associationOverview_associationStatusAggregatedCount' - Returns the number of targets for the association status. For example,
-- if you created an association with two managed nodes, and one of them
-- was successful, this would return the count of managed nodes by status.
--
-- 'detailedStatus', 'associationOverview_detailedStatus' - A detailed status of the association.
--
-- 'status', 'associationOverview_status' - The status of the association. Status can be: Pending, Success, or
-- Failed.
newAssociationOverview ::
  AssociationOverview
newAssociationOverview =
  AssociationOverview'
    { associationStatusAggregatedCount =
        Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Returns the number of targets for the association status. For example,
-- if you created an association with two managed nodes, and one of them
-- was successful, this would return the count of managed nodes by status.
associationOverview_associationStatusAggregatedCount :: Lens.Lens' AssociationOverview (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
associationOverview_associationStatusAggregatedCount = Lens.lens (\AssociationOverview' {associationStatusAggregatedCount} -> associationStatusAggregatedCount) (\s@AssociationOverview' {} a -> s {associationStatusAggregatedCount = a} :: AssociationOverview) Prelude.. Lens.mapping Lens.coerced

-- | A detailed status of the association.
associationOverview_detailedStatus :: Lens.Lens' AssociationOverview (Prelude.Maybe Prelude.Text)
associationOverview_detailedStatus = Lens.lens (\AssociationOverview' {detailedStatus} -> detailedStatus) (\s@AssociationOverview' {} a -> s {detailedStatus = a} :: AssociationOverview)

-- | The status of the association. Status can be: Pending, Success, or
-- Failed.
associationOverview_status :: Lens.Lens' AssociationOverview (Prelude.Maybe Prelude.Text)
associationOverview_status = Lens.lens (\AssociationOverview' {status} -> status) (\s@AssociationOverview' {} a -> s {status = a} :: AssociationOverview)

instance Data.FromJSON AssociationOverview where
  parseJSON =
    Data.withObject
      "AssociationOverview"
      ( \x ->
          AssociationOverview'
            Prelude.<$> ( x Data..:? "AssociationStatusAggregatedCount"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DetailedStatus")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AssociationOverview where
  hashWithSalt _salt AssociationOverview' {..} =
    _salt
      `Prelude.hashWithSalt` associationStatusAggregatedCount
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` status

instance Prelude.NFData AssociationOverview where
  rnf AssociationOverview' {..} =
    Prelude.rnf associationStatusAggregatedCount
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf status
