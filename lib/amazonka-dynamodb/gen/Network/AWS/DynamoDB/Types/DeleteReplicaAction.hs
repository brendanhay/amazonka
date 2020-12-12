{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicaAction
  ( DeleteReplicaAction (..),

    -- * Smart constructor
    mkDeleteReplicaAction,

    -- * Lenses
    draRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a replica to be removed.
--
-- /See:/ 'mkDeleteReplicaAction' smart constructor.
newtype DeleteReplicaAction = DeleteReplicaAction'
  { regionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicaAction' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region of the replica to be removed.
mkDeleteReplicaAction ::
  -- | 'regionName'
  Lude.Text ->
  DeleteReplicaAction
mkDeleteReplicaAction pRegionName_ =
  DeleteReplicaAction' {regionName = pRegionName_}

-- | The Region of the replica to be removed.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRegionName :: Lens.Lens' DeleteReplicaAction Lude.Text
draRegionName = Lens.lens (regionName :: DeleteReplicaAction -> Lude.Text) (\s a -> s {regionName = a} :: DeleteReplicaAction)
{-# DEPRECATED draRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON DeleteReplicaAction where
  toJSON DeleteReplicaAction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RegionName" Lude..= regionName)])
