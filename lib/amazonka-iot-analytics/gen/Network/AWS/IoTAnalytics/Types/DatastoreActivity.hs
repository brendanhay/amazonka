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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The datastore activity that specifies where to store the processed data.
--
-- /See:/ 'mkDatastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { name :: Lude.Text,
    datastoreName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatastoreActivity' with the minimum fields required to make a request.
--
-- * 'datastoreName' - The name of the data store where processed messages are stored.
-- * 'name' - The name of the datastore activity.
mkDatastoreActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'datastoreName'
  Lude.Text ->
  DatastoreActivity
mkDatastoreActivity pName_ pDatastoreName_ =
  DatastoreActivity'
    { name = pName_,
      datastoreName = pDatastoreName_
    }

-- | The name of the datastore activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DatastoreActivity Lude.Text
daName = Lens.lens (name :: DatastoreActivity -> Lude.Text) (\s a -> s {name = a} :: DatastoreActivity)
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the data store where processed messages are stored.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDatastoreName :: Lens.Lens' DatastoreActivity Lude.Text
daDatastoreName = Lens.lens (datastoreName :: DatastoreActivity -> Lude.Text) (\s a -> s {datastoreName = a} :: DatastoreActivity)
{-# DEPRECATED daDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

instance Lude.FromJSON DatastoreActivity where
  parseJSON =
    Lude.withObject
      "DatastoreActivity"
      ( \x ->
          DatastoreActivity'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "datastoreName")
      )

instance Lude.ToJSON DatastoreActivity where
  toJSON DatastoreActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("datastoreName" Lude..= datastoreName)
          ]
      )
