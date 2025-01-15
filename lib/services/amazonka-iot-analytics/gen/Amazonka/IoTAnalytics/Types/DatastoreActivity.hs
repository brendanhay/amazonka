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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The datastore activity that specifies where to store the processed data.
--
-- /See:/ 'newDatastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { -- | The name of the datastore activity.
    name :: Prelude.Text,
    -- | The name of the data store where processed messages are stored.
    datastoreName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'datastoreActivity_name' - The name of the datastore activity.
--
-- 'datastoreName', 'datastoreActivity_datastoreName' - The name of the data store where processed messages are stored.
newDatastoreActivity ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datastoreName'
  Prelude.Text ->
  DatastoreActivity
newDatastoreActivity pName_ pDatastoreName_ =
  DatastoreActivity'
    { name = pName_,
      datastoreName = pDatastoreName_
    }

-- | The name of the datastore activity.
datastoreActivity_name :: Lens.Lens' DatastoreActivity Prelude.Text
datastoreActivity_name = Lens.lens (\DatastoreActivity' {name} -> name) (\s@DatastoreActivity' {} a -> s {name = a} :: DatastoreActivity)

-- | The name of the data store where processed messages are stored.
datastoreActivity_datastoreName :: Lens.Lens' DatastoreActivity Prelude.Text
datastoreActivity_datastoreName = Lens.lens (\DatastoreActivity' {datastoreName} -> datastoreName) (\s@DatastoreActivity' {} a -> s {datastoreName = a} :: DatastoreActivity)

instance Data.FromJSON DatastoreActivity where
  parseJSON =
    Data.withObject
      "DatastoreActivity"
      ( \x ->
          DatastoreActivity'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "datastoreName")
      )

instance Prelude.Hashable DatastoreActivity where
  hashWithSalt _salt DatastoreActivity' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datastoreName

instance Prelude.NFData DatastoreActivity where
  rnf DatastoreActivity' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf datastoreName

instance Data.ToJSON DatastoreActivity where
  toJSON DatastoreActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datastoreName" Data..= datastoreName)
          ]
      )
