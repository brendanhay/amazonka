{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreActivity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The datastore activity that specifies where to store the processed data.
--
-- /See:/ 'newDatastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { -- | The name of the datastore activity.
    name :: Prelude.Text,
    -- | The name of the data store where processed messages are stored.
    datastoreName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DatastoreActivity where
  parseJSON =
    Prelude.withObject
      "DatastoreActivity"
      ( \x ->
          DatastoreActivity'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "datastoreName")
      )

instance Prelude.Hashable DatastoreActivity

instance Prelude.NFData DatastoreActivity

instance Prelude.ToJSON DatastoreActivity where
  toJSON DatastoreActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just
              ("datastoreName" Prelude..= datastoreName)
          ]
      )
