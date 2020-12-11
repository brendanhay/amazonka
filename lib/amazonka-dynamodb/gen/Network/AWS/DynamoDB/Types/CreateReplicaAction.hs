-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicaAction
  ( CreateReplicaAction (..),

    -- * Smart constructor
    mkCreateReplicaAction,

    -- * Lenses
    craRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a replica to be added.
--
-- /See:/ 'mkCreateReplicaAction' smart constructor.
newtype CreateReplicaAction = CreateReplicaAction'
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

-- | Creates a value of 'CreateReplicaAction' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region of the replica to be added.
mkCreateReplicaAction ::
  -- | 'regionName'
  Lude.Text ->
  CreateReplicaAction
mkCreateReplicaAction pRegionName_ =
  CreateReplicaAction' {regionName = pRegionName_}

-- | The Region of the replica to be added.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRegionName :: Lens.Lens' CreateReplicaAction Lude.Text
craRegionName = Lens.lens (regionName :: CreateReplicaAction -> Lude.Text) (\s a -> s {regionName = a} :: CreateReplicaAction)
{-# DEPRECATED craRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON CreateReplicaAction where
  toJSON CreateReplicaAction' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RegionName" Lude..= regionName)])
