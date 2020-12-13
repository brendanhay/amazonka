{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTable
  ( GlobalTable (..),

    -- * Smart constructor
    mkGlobalTable,

    -- * Lenses
    gtGlobalTableName,
    gtReplicationGroup,
  )
where

import Network.AWS.DynamoDB.Types.Replica
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a global table.
--
-- /See:/ 'mkGlobalTable' smart constructor.
data GlobalTable = GlobalTable'
  { -- | The global table name.
    globalTableName :: Lude.Maybe Lude.Text,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Lude.Maybe [Replica]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalTable' with the minimum fields required to make a request.
--
-- * 'globalTableName' - The global table name.
-- * 'replicationGroup' - The Regions where the global table has replicas.
mkGlobalTable ::
  GlobalTable
mkGlobalTable =
  GlobalTable'
    { globalTableName = Lude.Nothing,
      replicationGroup = Lude.Nothing
    }

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtGlobalTableName :: Lens.Lens' GlobalTable (Lude.Maybe Lude.Text)
gtGlobalTableName = Lens.lens (globalTableName :: GlobalTable -> Lude.Maybe Lude.Text) (\s a -> s {globalTableName = a} :: GlobalTable)
{-# DEPRECATED gtGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The Regions where the global table has replicas.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtReplicationGroup :: Lens.Lens' GlobalTable (Lude.Maybe [Replica])
gtReplicationGroup = Lens.lens (replicationGroup :: GlobalTable -> Lude.Maybe [Replica]) (\s a -> s {replicationGroup = a} :: GlobalTable)
{-# DEPRECATED gtReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

instance Lude.FromJSON GlobalTable where
  parseJSON =
    Lude.withObject
      "GlobalTable"
      ( \x ->
          GlobalTable'
            Lude.<$> (x Lude..:? "GlobalTableName")
            Lude.<*> (x Lude..:? "ReplicationGroup" Lude..!= Lude.mempty)
      )
