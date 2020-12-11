-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Replica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Replica
  ( Replica (..),

    -- * Smart constructor
    mkReplica,

    -- * Lenses
    rRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a replica.
--
-- /See:/ 'mkReplica' smart constructor.
newtype Replica = Replica' {regionName :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Replica' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region where the replica needs to be created.
mkReplica ::
  Replica
mkReplica = Replica' {regionName = Lude.Nothing}

-- | The Region where the replica needs to be created.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegionName :: Lens.Lens' Replica (Lude.Maybe Lude.Text)
rRegionName = Lens.lens (regionName :: Replica -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: Replica)
{-# DEPRECATED rRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.FromJSON Replica where
  parseJSON =
    Lude.withObject
      "Replica"
      (\x -> Replica' Lude.<$> (x Lude..:? "RegionName"))

instance Lude.ToJSON Replica where
  toJSON Replica' {..} =
    Lude.object
      (Lude.catMaybes [("RegionName" Lude..=) Lude.<$> regionName])
