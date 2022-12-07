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
-- Module      : Amazonka.SSMIncidents.Types.DeleteRegionAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.DeleteRegionAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the information about the Amazon Web Services Region you\'re
-- deleting from your replication set.
--
-- /See:/ 'newDeleteRegionAction' smart constructor.
data DeleteRegionAction = DeleteRegionAction'
  { -- | The name of the Amazon Web Services Region you\'re deleting from the
    -- replication set.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegionAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'deleteRegionAction_regionName' - The name of the Amazon Web Services Region you\'re deleting from the
-- replication set.
newDeleteRegionAction ::
  -- | 'regionName'
  Prelude.Text ->
  DeleteRegionAction
newDeleteRegionAction pRegionName_ =
  DeleteRegionAction' {regionName = pRegionName_}

-- | The name of the Amazon Web Services Region you\'re deleting from the
-- replication set.
deleteRegionAction_regionName :: Lens.Lens' DeleteRegionAction Prelude.Text
deleteRegionAction_regionName = Lens.lens (\DeleteRegionAction' {regionName} -> regionName) (\s@DeleteRegionAction' {} a -> s {regionName = a} :: DeleteRegionAction)

instance Prelude.Hashable DeleteRegionAction where
  hashWithSalt _salt DeleteRegionAction' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData DeleteRegionAction where
  rnf DeleteRegionAction' {..} = Prelude.rnf regionName

instance Data.ToJSON DeleteRegionAction where
  toJSON DeleteRegionAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("regionName" Data..= regionName)]
      )
