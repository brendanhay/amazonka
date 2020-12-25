{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreActivity
  ( DatastoreActivity (..),

    -- * Smart constructor
    mkDatastoreActivity,

    -- * Lenses
    daName,
    daDatastoreName,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The datastore activity that specifies where to store the processed data.
--
-- /See:/ 'mkDatastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { -- | The name of the datastore activity.
    name :: Types.ActivityName,
    -- | The name of the data store where processed messages are stored.
    datastoreName :: Types.DatastoreName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatastoreActivity' value with any optional fields omitted.
mkDatastoreActivity ::
  -- | 'name'
  Types.ActivityName ->
  -- | 'datastoreName'
  Types.DatastoreName ->
  DatastoreActivity
mkDatastoreActivity name datastoreName =
  DatastoreActivity' {name, datastoreName}

-- | The name of the datastore activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DatastoreActivity Types.ActivityName
daName = Lens.field @"name"
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the data store where processed messages are stored.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDatastoreName :: Lens.Lens' DatastoreActivity Types.DatastoreName
daDatastoreName = Lens.field @"datastoreName"
{-# DEPRECATED daDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

instance Core.FromJSON DatastoreActivity where
  toJSON DatastoreActivity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("datastoreName" Core..= datastoreName)
          ]
      )

instance Core.FromJSON DatastoreActivity where
  parseJSON =
    Core.withObject "DatastoreActivity" Core.$
      \x ->
        DatastoreActivity'
          Core.<$> (x Core..: "name") Core.<*> (x Core..: "datastoreName")
